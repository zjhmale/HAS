{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Database.Persist.MySQL               hiding (delete, update,
                                                       (=.), (==.))
import Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

connectionInfo :: Environment -> ConnectInfo
connectionInfo env = defaultConnectInfo
  { connectPort = 3306
  , connectHost = "127.0.0.1"
  , connectUser = "root"
  , connectPassword = "cleantha"
  , connectDatabase = envDbName env
  }

data Config = Config
  { getPool :: ConnectionPool
  , getEnv  :: Environment
  }

data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

defaultConfig :: Config
defaultConfig = Config
  { getPool = undefined
  , getEnv  = Development
  }

setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

envDbName :: Environment -> String
envDbName Test = "todo_servant_test"
envDbName _ = "todo_servant"

envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

makePool :: Environment -> IO ConnectionPool
makePool Test = runNoLoggingT $ createMySQLPool (connectionInfo Test) (envPool Test)
makePool e = runStdoutLoggingT $ createMySQLPool (connectionInfo e) (envPool e)
