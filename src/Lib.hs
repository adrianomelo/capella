{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( someFunc
    ) where

import Control.Applicative ((<$>))
import Control.Lens (set, view, (&), (<&>), (?~), (^.))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.AWS
  ( Credentials (Discover),
    HasEnv (envLogger),
    LogLevel (Debug),
    Region(Frankfurt),
    newEnv,
    newLogger,
    runAWST,
    runResourceT,
    send,
    within,
  )
-- import Data.Aeson
--   ( ToJSON,
--     fromEncoding,
--     object,
--     toEncoding,
--     (.=),
--   )
import qualified Data.Binary
import qualified Data.Binary.Builder as Builder
-- import qualified Data.ByteString.Lazy.UTF8 as BLU
--import qualified Data.ByteString.Lex.Integral as Lex
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.String.Conversions (cs)
import Data.Aeson ( FromJSON, ToJSON (toEncoding), encode )
import Data.Monoid (Monoid (mempty), (<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import GHC.Base ((.))
import qualified Network.AWS.SNS as SNS
import Network.AWS.SQS
  ( createQueue,
    getQueueURL,
    gqursQueueURL,
    receiveMessage,
    rmWaitTimeSeconds,
    rmrsMessages,
    sendMessage,
  )
import Network.HTTP.Types
  ( hContentType,
    status200,
    status201,
    status400,
    status404,
    status405,
  )
import Network.Socket (SockAddr)
import Network.SockAddr (showSockAddr)
import Network.Wai
  ( Application,
    Request
      ( pathInfo,
        remoteHost,
        requestHeaderReferer,
        requestHeaderUserAgent
      ),
    Response,
    responseBuilder,
    responseFile,
  )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Autohead (autohead)
import qualified Network.Wai.Middleware.Prometheus as P
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Parse
  ( lbsBackEnd,
    parseRequestBody,
  )
import RIO
  ( Applicative (pure),
    IO,
    Int,
    Map,
    Maybe (..),
    Monoid (mempty),
    Show,
    TVar,
    Text,
    newTVarIO,
    ($),
  )
import qualified RIO.Map as Map
import System.IO (IO, print, stdout)
import Prelude (show)
import GHC.Generics (Generic)
import Control.Monad (return)

-- Type synonyms for nicer signatures below
type Name = Text
type Age = Int
type PeopleMap = Map Name Age
type PeopleVar = TVar PeopleMap

data TrackingEvent = TrackingEvent
  { uuid :: Text
  , remote :: Text -- SockAddr
  , referer :: Text
  , userAgent :: Text
  } deriving (Show, Generic)

instance FromJSON TrackingEvent
instance ToJSON TrackingEvent

-- Common error responses
notFound :: Network.Wai.Response
notFound = responseBuilder status404 [] "Not found"

badRequest :: Network.Wai.Response
badRequest = responseBuilder status405 [] "Bad request method"

parsePath :: Text -> Maybe UUID.UUID
parsePath path =
  UUID.fromText (Text.take 36 path)

buildEvent :: Network.Wai.Request -> UUID.UUID -> TrackingEvent
buildEvent req uid =
  TrackingEvent uuid remote referer userAgent
  where
    uuid = UUID.toText uid
    remote = Text.pack $ showSockAddr $ remoteHost req
    referer = Maybe.maybe "" Text.decodeUtf8 $ requestHeaderReferer req
    userAgent = Maybe.maybe "" Text.decodeUtf8 $ requestHeaderUserAgent req

roundTrip :: Region -- ^ Region to operate in.
         -> Text   -- ^ Name of the queue to create.
         -> Text -- ^ Contents of the messages to send.
         -> IO ()
roundTrip r name message = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr

    runResourceT . runAWST env . within r $ do
        url <- view gqursQueueURL <$> send (getQueueURL name)
        void $ send (sendMessage url message)

simpleImageApp :: PeopleVar -> Application
simpleImageApp var req send = do
  result <-
    case pathInfo req of
      [path] ->
        case parsePath path of
          Just uuid -> pure $ Either.Right $ buildEvent req uuid
          _ -> pure $ Either.Left "Incorrect file name"
      _ -> pure $ Either.Left "Invalid path"

  case result of
    Either.Right b ->
      print b
    _ ->
      mempty

  case result of
    Either.Right b ->
      roundTrip Frankfurt "test" $ cs $ encode b
    _ ->
      mempty

  response <-
    case result of
      Either.Right event ->
        pure $ responseFile status200 [("Cache-Control", "no-cache")] "small.gif" Nothing
      Either.Left message ->
        pure badRequest

  send response

someFunc :: IO ()
someFunc = do
  peopleVar <- newTVarIO mempty
  run 8000 $ P.prometheus P.def { P.prometheusEndPoint = ["metrics"] } $ simpleImageApp peopleVar
