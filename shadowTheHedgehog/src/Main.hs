module Main where

import API
import Network.Wai.Handler.Warp

main :: IO ()
main = do
    run 8080 routeApi
