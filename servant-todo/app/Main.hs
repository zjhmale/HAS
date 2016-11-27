module Main where

import           Config
import           Database.Persist.MySQL   (runSqlPool)
import           Model
import           Network.Wai.Handler.Warp (run)
import           Route                    (app)
import           System.Environment       (lookupEnv)

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
