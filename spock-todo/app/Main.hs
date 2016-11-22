module Main where

import           Route (run)
import           System.Environment                   (lookupEnv)
import Config

main :: IO ()
main = do
  env  <- maybe Development read <$> lookupEnv "ENV"
  port <- maybe 3000 read <$> lookupEnv "PORT"
  run port env
