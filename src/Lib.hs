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

-- | Build a successful JSON response
jsonResponse :: ToJSON a => a -> Response
jsonResponse
  = responseBuilder status200 [(hContentType, "application/json")]
  . fromEncoding . toEncoding

peopleApp :: PeopleVar -> Application
peopleApp peopleVar req send = do
  response <-
    case pathInfo req of
      ["people"] ->
        case requestMethod req of
          "GET"  -> getPeopleResponse peopleVar
          "POST" -> postPeopleResponse peopleVar req
          _      -> pure badRequest
      ["person", name] ->
        case requestMethod req of
          "GET" -> getPersonResponse peopleVar name
          "PUT" -> do
            let ageParam = lookup "age" $ queryString req
            putPersonResponse peopleVar name ageParam
      _ -> pure notFound
  send response

getPeopleResponse :: PeopleVar -> IO Response
getPeopleResponse peopleVar = do
  people <- atomically $ readTVar peopleVar
  pure $ jsonResponse $ Map.keys people

postPeopleResponse :: PeopleVar -> Request -> IO Response
postPeopleResponse peopleVar req = do
  (params, _) <- parseRequestBody lbsBackEnd req
  let mpair = do
        nameBS <- lookup "name" params
        name <- either (const Nothing) Just $ decodeUtf8' nameBS
        ageBS <- lookup "age" params
        (age, "") <- Lex.readDecimal ageBS
        Just (name, age)
  case mpair of
    Just (name, age) -> do
      atomically $ modifyTVar' peopleVar $ Map.insert name age
      pure $ responseBuilder status201 [] ""
    Nothing -> pure $ responseBuilder status400 [] "Invalid parameters"

getPersonResponse :: PeopleVar -> Name -> IO Response
getPersonResponse peopleVar name = do
  people <- atomically $ readTVar peopleVar
  case Map.lookup name people of
    Nothing -> pure notFound
    Just age -> pure $ jsonResponse $ object
      [ "name" .= name
      , "age" .= age
      ]

putPersonResponse :: PeopleVar -> Name -> Maybe (Maybe ByteString) -> IO Response
putPersonResponse _ _ Nothing = pure $ responseBuilder status400 [] "No age parameter"
putPersonResponse _ _ (Just Nothing) = pure $ responseBuilder status400 [] "Empty age parameter"
putPersonResponse peopleVar name (Just (Just bs)) =
  case Lex.readDecimal bs of
    Just (age, "") -> do
      atomically $ modifyTVar' peopleVar $ Map.insert name age
      pure $ responseBuilder status201 [] ""
    _ -> pure $ responseBuilder status400 [] "Invalid age parameter"

someFunc :: IO ()
someFunc = do
  peopleVar <- newTVarIO mempty
  -- logStdout, autohead
  run 8000 $ P.prometheus P.def { P.prometheusEndPoint = ["metrics"] } $ peopleApp peopleVar

