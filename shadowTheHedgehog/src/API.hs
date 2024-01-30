{-# LANGUAGE OverloadedStrings #-}

module API where

import Data.Aeson
import Data.String
import Data.Text (Text)
import Data.Void
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

import ShadowTheHedgehog (routeFromNum, allRoutes)

routeApi :: Application
routeApi req respond =
    let path = pathInfo req
     in case path of
          [] -> respond indexHTML
          ["index.html"] -> respond indexHTML
          ["index.js"] -> respond indexJS
          ["api", "v1", "routes", routeNum] ->
              respond $ maybe response404 routeResponseFromNumber $ parseInt routeNum
          _ -> 
              respond response404

indexHTML :: Response
indexHTML = responseFile
    status200
    [("Content-Type", "text/html")]
    "static/index.html"
    Nothing

indexJS :: Response
indexJS = responseFile
    status200
    [("Content-Type", "text/javascript")]
    "static/index.js"
    Nothing

routeResponseFromNumber :: Int -> Response
routeResponseFromNumber n =
    if n < 1 || n > 326
       then response404
       else responseLBS
            status200 
            [contentTypeJson]
            (encode (routeFromNum n))

response404 :: Response
response404 = responseLBS
    status404
    [contentTypeJson]
    "{\"status\": 404, \"statusText\": \"Not Found\"}"

contentTypeJson :: Header
contentTypeJson = ("Content-Type", "application/json")

parseInt :: Text -> Maybe Int 
parseInt = MP.parseMaybe intParser where
    intParser :: MP.Parsec Void Text Int
    intParser = MPL.decimal
