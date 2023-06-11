-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( someFunc
    ) where

import Data.Maybe as Maybe ( maybe )
import Data.Text as Text ( Text, take )
import Data.UUID ( fromText, UUID )
import Network.HTTP.Types ( status200, status405 )
import Network.Wai
    ( responseBuilder,
      responseFile,
      Application,
      Request(pathInfo),
      Response )
import Network.Wai.Handler.Warp (run)
-- import qualified Network.Wai.Middleware.Prometheus as P
import qualified Middleware as P

-- data TrackingEvent = TrackingEvent
--   { uuid :: Text
--   , remote :: Text -- SockAddr
--   , referer :: Text
--   , userAgent :: Text
--   } deriving (Show, Generic)

badRequest :: Network.Wai.Response
badRequest = responseBuilder status405 [] "Bad request method"

validatePath :: [Text] -> Either Text UUID
validatePath (fileName:_) = maybe notUuid Right uuid
  where
    id = Text.take 36 fileName
    uuid = fromText id
    notUuid = Left "File format not recognized"
    -- isGif = Text.isSuffixOf "gif" ext 

-- buildEvent :: Network.Wai.Request -> UUID.UUID -> TrackingEvent
-- buildEvent req uid =
--   TrackingEvent uuid remote referer userAgent
--   where
--     uuid = UUID.toText uid
--     remote = Text.pack $ showSockAddr $ remoteHost req
--     referer = Maybe.maybe "" Text.decodeUtf8 $ requestHeaderReferer req
--     userAgent = Maybe.maybe "" Text.decodeUtf8 $ requestHeaderUserAgent req

simpleImageApp ::  Application
simpleImageApp req send = do

  let validPath = validatePath . pathInfo $ req

  case validPath of
    Right uuid ->
      send $ responseFile status200 [("Cache-Control", "no-cache")] "small.gif" Nothing

    Left msg ->
      send badRequest

someFunc :: IO ()
someFunc = do
  run 8000 $ P.prometheus P.def { P.prometheusEndPoint = ["metrics"], P.prometheusInstrumentPrometheus = False } simpleImageApp
