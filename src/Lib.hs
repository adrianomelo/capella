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
import qualified Middleware as P


badRequest :: Network.Wai.Response
badRequest = responseBuilder status405 [] "Bad request method"

validatePath :: [Text] -> Either Text UUID
validatePath (fileName:_) = maybe notUuid Right uuid
  where
    id = Text.take 36 fileName
    uuid = fromText id
    notUuid = Left "File format not recognized"
    -- isGif = Text.isSuffixOf "gif" ext 

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
