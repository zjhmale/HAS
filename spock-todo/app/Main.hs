module Main where

import           Config
import           Paths_spock_todo   (version)
import           Route              (run)
-- https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-data-files-from-package-code
import           System.Environment (lookupEnv)

main :: IO ()
main = do
  print version
  env  <- maybe Development read <$> lookupEnv "ENV"
  port <- maybe 3000 read <$> lookupEnv "PORT"
  run port env
