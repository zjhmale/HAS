module Main where

import           Network.Wai.Handler.Warp (run)
import           Route                    (app)
import           System.Environment (lookupEnv)
import Model
import Config
import Database.Persist.MySQL (runSqlPool)

main :: IO ()
main = do
  env  <- maybe Development read <$> lookupEnv "ENV"
  port <- maybe 8080 read <$> lookupEnv "PORT"
  pool <- makePool env
  let cfg = Config { getPool = pool
                   , getEnv = env
                   }
      logger = setLogger env
  runSqlPool doMigrations pool
  putStrLn $ "Server is running on port " ++ show port
  run port $ logger $ app cfg
