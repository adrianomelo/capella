-- | This module provides "Network.Wai" middlware for exporting "Prometheus"
-- metrics and for instrumenting WAI applications.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Middleware
  ( prometheus
  , PrometheusSettings(..)
  , Default.def
  , instrumentHandlerValue
  , instrumentHandlerValueWithFilter
  , ignoreRawResponses
  , instrumentApp
  , instrumentIO
  , metricsApp
  ) where

import qualified Data.Default as Default
import Data.Maybe (fromMaybe, fromJust)
import Data.Ratio ((%))
import Data.Text (Text,intercalate)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai (Response(ResponseRaw))
import qualified Prometheus as Prom
import System.Clock (Clock(..), TimeSpec, diffTimeSpec, getTime, toNanoSecs)
import Prometheus (incCounter)
import Web.UAParser ( parseUA, parseOS, parseDev, osrVersion, uarVersion, UAResult (uarFamily), OSResult (osrFamily), DevResult (..) )
import qualified Network.HTTP.Types as Wai


-- | Settings that control the behavior of the Prometheus middleware.
data PrometheusSettings = PrometheusSettings {
        prometheusEndPoint             :: [T.Text]
        -- ^ The path that will be used for exporting metrics. The default value
        -- is ["metrics"] which corresponds to the path /metrics.
    ,   prometheusInstrumentApp        :: Bool
        -- ^ Whether the default instrumentation should be applied to the
        -- application. If this is set to false the application can still be
        -- instrumented using the 'instrumentApp' function. The default value is
        -- True.
    ,   prometheusInstrumentPrometheus :: Bool
        -- ^ Whether the default instrumentation should be applied to the
        -- middleware that serves the metrics endpoint. The default value is
        -- True.
    }

instance Default.Default PrometheusSettings where
    def = PrometheusSettings {
        prometheusEndPoint             = ["metrics"]
    ,   prometheusInstrumentApp        = True
    ,   prometheusInstrumentPrometheus = True
    }

responseFilter res
  | isSuccessful = Just res
  | otherwise = Nothing
  where
    isSuccessful = (== Wai.status200) . Wai.responseStatus $ res

-- | This function is used to populate the @handler@ label of all Prometheus metrics recorded by this library.
--
-- If you use this function you will likely want to override the default value
-- of 'prometheusInstrumentApp' to be false so that your app does not get double
-- instrumented.
--
-- WARNING: If you have 'ResponseRaw' values in your API, consider using
-- @instrumentHandlerValueWithFilter ignoreRawResponses@ instead.
instrumentHandlerValue ::
     (Wai.Request -> Text) -- ^ The function used to derive the "handler" value in Prometheus
  -> Wai.Application -- ^ The app to instrument
  -> Wai.Application -- ^ The instrumented app
instrumentHandlerValue = instrumentHandlerValueWithFilter responseFilter

-- | A more flexible variant of 'instrumentHandlerValue'.  The filter can change some
-- responses, or drop others entirely.
instrumentHandlerValueWithFilter ::
     (Wai.Response -> Maybe Wai.Response) -- ^ Response filter
  -> (Wai.Request -> Text) -- ^ The function used to derive the "handler" value in Prometheus
  -> Wai.Application -- ^ The app to instrument
  -> Wai.Application -- ^ The instrumented app
instrumentHandlerValueWithFilter resFilter f app req respond = do
  -- start <- getTime Monotonic
  app req $ \res -> do
    case resFilter res of
      Nothing -> return ()
      Just res' -> do
        -- end <- getTime Monotonic
        let method = Just $ decodeUtf8 (Wai.requestMethod req)
        let status = Just $ T.pack (show (HTTP.statusCode (Wai.responseStatus res')))
        let path = Just $ intercalate "/" (Wai.pathInfo req)
        let referer = decodeUtf8 <$> Wai.requestHeaderReferer req
        let userAgentHeader = Wai.requestHeaderUserAgent req

        let ua = parseUA =<< userAgentHeader
        let uaFamily = uarFamily <$> ua
        let uaVersion = uarVersion <$> ua

        let os = parseOS =<< userAgentHeader
        let osFamily = osrFamily <$> os
        let osVersion = osrVersion <$> os
        
        observeUserAgents path uaFamily uaVersion osFamily osVersion
        observePageViews path referer
    respond res

{-# NOINLINE userAgentMetric #-}
userAgentMetric :: Prom.Vector Prom.Label5 Prom.Counter
userAgentMetric = Prom.unsafeRegister $ Prom.vector ("app", "user_agent", "user_agent_version", "operating_system", "operating_system_version")
                                     $ Prom.counter info
    where info = Prom.Info "http_user_agents_total"
                           "Total number of times an user agent visited a page."

observeUserAgents :: Maybe Text   -- ^ app
               -> Maybe Text   -- ^ ua family
               -> Maybe Text   -- ^ ua version
               -> Maybe Text   -- ^ os family
               -> Maybe Text   -- ^ os version
               -> IO ()
observeUserAgents app uaFamily uaVersion osFamily osVersion = do
    Prom.withLabel userAgentMetric (fromMaybe "" app, fromMaybe "" uaFamily, fromMaybe "" uaVersion, fromMaybe "" osFamily, fromMaybe "" osVersion) Prom.incCounter

{-# NOINLINE pageViewMetric #-}
pageViewMetric :: Prom.Vector Prom.Label2 Prom.Counter
pageViewMetric = Prom.unsafeRegister $ Prom.vector ("app", "page")
                                     $ Prom.counter info
    where info = Prom.Info "http_page_views_total"
                           "Total number of times a page was viewed."

observePageViews :: Maybe Text   -- ^ app
               -> Maybe Text   -- ^ referer
               -> IO ()
observePageViews app page = do
    Prom.withLabel pageViewMetric (fromMaybe "" app, fromMaybe "" page) Prom.incCounter

-- | 'Wai.ResponseRaw' values have two parts: an action that can be executed to construct a
-- 'Wai.Response', and a pure "backup" 'Wai.Response' in case the computation fails.  Since
-- the pure selectors like 'Wai.responseStatus' are pure and it makes no sense for them to
-- call the action, they just go to the backup response and pull the value from that:
--
-- @
-- responseStatus (ResponseRaw ...
--   (ResponseBuilder (Status 500 "blargh") ... ...))
-- == Status {statusCode = 500, statusMessage = "blargh"}
-- @
--
-- This is often not what you want.  For example, if you have an end-point for establishing
-- websocket connections that has a backup response with status 5xx, every websocket
-- connection request, whether successful or not, will register as an internal server error.
--
-- This helper therefore filters out all raw requests so they won't create any metrics.  Use
-- together with 'instrumentHandlerValueWithFilter'.
ignoreRawResponses :: Wai.Response -> Maybe Wai.Response
ignoreRawResponses (Wai.ResponseRaw {}) = Nothing
ignoreRawResponses res = Just res

-- | Instrument a WAI app with the default WAI metrics.
--
-- If you use this function you will likely want to override the default value
-- of 'prometheusInstrumentApp' to be false so that your app does not get double
-- instrumented.
instrumentApp ::
     Text -- ^ The label used to identify this app
  -> Wai.Application -- ^ The app to instrument
  -> Wai.Application -- ^ The instrumented app
instrumentApp handler app req respond =
  instrumentHandlerValue (const handler) app req respond

-- | Instrument an IO action with timing metrics. This function can be used if
-- you would like to get more fine grained metrics, for instance this can be
-- used to instrument individual end points.
--
-- If you use this function you will likely want to override the default value
-- of 'prometheusInstrumentApp' to be false so that your app does not get double
-- instrumented.
instrumentIO :: Text    -- ^ The label used to identify this IO operation
             -> IO a    -- ^ The IO action to instrument
             -> IO a    -- ^ The instrumented app
instrumentIO label io = do
    start  <- getTime Monotonic
    result <- io
    end    <- getTime Monotonic
    observePageViews Nothing Nothing
    return result

-- | Expose Prometheus metrics and instrument an application with some basic
-- metrics (e.g. request latency).
prometheus :: PrometheusSettings -> Wai.Middleware
prometheus PrometheusSettings{..} app req respond =
    if     Wai.requestMethod req == HTTP.methodGet
        && Wai.pathInfo req == prometheusEndPoint
        -- XXX: Should probably be "metrics" rather than "prometheus", since
        -- "prometheus" can be confused with actual prometheus.
    then
      if prometheusInstrumentPrometheus
        then instrumentApp "prometheus" (const respondWithMetrics) req respond
        else respondWithMetrics respond
    else
      if prometheusInstrumentApp
        then instrumentApp "app" app req respond
        else app req respond


-- | WAI Application that serves the Prometheus metrics page regardless of
-- what the request is.
metricsApp :: Wai.Application
metricsApp = const respondWithMetrics

respondWithMetrics :: (Wai.Response -> IO Wai.ResponseReceived)
                   -> IO Wai.ResponseReceived
respondWithMetrics respond = do
    metrics <- Prom.exportMetricsAsText
    respond $ Wai.responseLBS HTTP.status200 headers metrics
    where
        headers = [(HTTP.hContentType, "text/plain; version=0.0.4")]
