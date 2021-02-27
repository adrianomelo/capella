{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           Data.Aeson                           (ToJSON, fromEncoding,
                                                       object, toEncoding, (.=))
import qualified Data.ByteString.Lex.Integral         as Lex
import           Network.HTTP.Types                   (hContentType, status200,
                                                       status201, status400,
                                                       status404, status405)
import           Network.Wai 
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.Autohead      (autohead)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai.Parse                    (lbsBackEnd,
                                                       parseRequestBody)
import           RIO
import qualified RIO.Map                              as Map

import qualified Network.Wai.Middleware.Prometheus as P

import qualified Data.List as List
import System.IO
import qualified Data.ByteString.Lazy.UTF8  as BLU
import qualified Data.Binary.Builder as Builder
import qualified Data.Binary
import qualified Data.UUID as UUID
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import Network.Socket (SockAddr)

-- Type synonyms for nicer signatures below
type Name = Text
type Age = Int
type PeopleMap = Map Name Age
type PeopleVar = TVar PeopleMap

data TrackingEvent = TrackingEvent
  { uuid :: Text
  , remote :: SockAddr
  , referer :: Maybe ByteString
  , userAgent :: Maybe ByteString
  } deriving (Show) --, Generic)

--instance FromJSON TrackingEvent
--instance ToJSON TrackingEvent

-- Common error responses
notFound :: Response
notFound = responseBuilder status404 [] "Not found"

badRequest :: Response
badRequest = responseBuilder status405 [] "Bad request method"

parsePath :: Text -> Maybe UUID.UUID
parsePath path =
  UUID.fromText (Text.take 36 path)

buildEvent :: Request -> UUID.UUID -> TrackingEvent
buildEvent req uid =
  TrackingEvent uuid remote referer userAgent
  where
    uuid = UUID.toText uid
    remote = remoteHost req
    referer = requestHeaderReferer req
    userAgent = requestHeaderUserAgent req

simpleImageApp :: PeopleVar -> Application
simpleImageApp var req send = do
  result <-
    case pathInfo req of
      [path] ->
        case parsePath path of
          Just uuid -> pure $ Either.Right $ buildEvent req uuid
          _ -> pure $ Either.Left "Incorrect file name"
      _ -> pure $ Either.Left "Invalid path"

  print result

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

