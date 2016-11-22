{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Database.Persist.MySQL               hiding (delete, update,
                                                       (=.), (==.))
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
  { sqlHandler :: SqlBackend
  , getEnv     :: Environment
  }

data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

envDbName :: Environment -> String
envDbName Test = "todo_test"
envDbName _ = "todo"

envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8
