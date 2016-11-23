module Main where

import           Network.Wai.Handler.Warp
import           Route                    (app)

main :: IO ()
main = run 8080 app
