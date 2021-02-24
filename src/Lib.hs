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
import           Network.Wai                          (Application, Request,
                                                       Response, pathInfo,
                                                       queryString,
                                                       rawPathInfo,
                                                       responseFile,
                                                       requestMethod,
                                                       responseBuilder)
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

-- Type synonyms for nicer signatures below
type Name = Text
type Age = Int
type PeopleMap = Map Name Age
type PeopleVar = TVar PeopleMap

-- Common error responses
notFound :: Response
notFound = responseBuilder status404 [] "Not found"

badRequest :: Response
badRequest = responseBuilder status405 [] "Bad request method"

simpleImageApp :: PeopleVar -> Application
simpleImageApp var req send = do
  response <-
    case pathInfo req of
      [path] -> pure $ responseFile status200 [("Cache-Control", "no-cache")] "small.gif" Nothing

      _ -> pure badRequest

  send response

someFunc :: IO ()
someFunc = do
  peopleVar <- newTVarIO mempty
  run 8000 $ P.prometheus P.def { P.prometheusEndPoint = ["metrics"] } $ simpleImageApp peopleVar

