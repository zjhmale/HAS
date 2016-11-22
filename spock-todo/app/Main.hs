module Main where

import           Config
import           Route              (run)
import           System.Environment (lookupEnv)

main :: IO ()
main = do
  env  <- maybe Development read <$> lookupEnv "ENV"
  port <- maybe 3000 read <$> lookupEnv "PORT"
  run port env
