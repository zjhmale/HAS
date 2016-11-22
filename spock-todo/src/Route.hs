{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Route where

import           Config
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource         (runResourceT)
import           Controller
import           Data.Aeson                           (object, (.=))
import           Data.Int                             (Int64)
import           Database.Persist.MySQL               hiding (delete, get,
                                                       update, (=.), (==.))
import           Model
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           Web.Spock
import           Web.Spock.Config

getAppCfg :: Environment -> IO (SpockCfg SqlBackend (Maybe (Int64, User)) ())
getAppCfg env = do
  sessionCfg <- defaultSessionCfg Nothing
  let sessionCfg' = sessionCfg { sc_cookieName = "todo"
                               , sc_sessionTTL = 604800 -- one week
                               }

  pool <- runStdoutLoggingT $ createMySQLPool (connectionInfo env) (envPool env)
  runResourceT . runStdoutLoggingT . liftIO $ runSqlPersistMPool (runMigration migrateAll) pool
  appCfg <- defaultSpockCfg Nothing (PCPool pool) ()
  return $ appCfg { spc_maxRequestSize = Just (5 * 1024 * 1024)
                  , spc_sessionCfg = sessionCfg'
                  }

run :: Int -> Environment -> IO ()
run port env = do
  appCfg <- getAppCfg env
  runSpock port $ spockApp env appCfg

spockApp :: Environment -> SpockCfg SqlBackend (Maybe (Int64, User)) state
         -> IO Middleware
spockApp env cfg = spock cfg $ app env

app :: Environment -> SpockM SqlBackend (Maybe (Int64, User)) state ()
app env = do
    middleware logStdoutDev
    let onfail = json $ object ["ok" .= False, "err" .= ("Session expired." :: String)]

    get root $ let msg = "welcome" :: String in json $ object ["msg" .= msg]

    post "posts" $
      requireAuth onfail $ \(uid, _) ->
        jsonBody' >>= runQuery' . newPost uid >>= json

    put ("posts" <//> var) $ \pid ->
      requireAuth onfail $ \(uid, _) ->
      jsonBody' >>= runQuery' . editPost pid uid >>= json

    delete ("posts" <//> var) $ \pid ->
      requireAuth onfail $ \(uid, _) -> do
      r <- runQuery' $ removePost pid uid
      json r

    get "posts" $
      requireAuth onfail $ \_ ->
      runQuery' getPosts >>= json

    get ("posts" <//> var) $ \pid ->
      requireAuth onfail $ \_ ->
      runQuery' (getPost pid) >>= json

    post "login" $ do
      username <- param' "username"
      password <- param' "password"
      _type <- param' "type"
      (j, sess) <- runQuery' $ loginSignupPost username password _type
      writeSession sess
      json j

    get "logout" $ do
      writeSession Nothing
      redirect "/login"

  where
    requireAuth onfail action = do
      sess <- readSession
      case sess of
        Nothing -> onfail
        Just user -> action user

    runQuery' action = do
      _ <- readSession
      runQuery $ \conn ->
        liftIO (runReaderT action (Config conn env))
