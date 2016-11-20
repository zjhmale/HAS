{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Aeson (object, (.=))
import Data.Int (Int64)
import Control.Monad.Reader
import System.Environment (lookupEnv)
import Web.Spock
import Web.Spock.Config
import Network.Wai.Middleware.RequestLogger
import Model
import Controller

main :: IO ()
main = runConnPool $ \pool -> do
  sessionCfg <- defaultSessionCfg Nothing
  let sessionCfg' = sessionCfg { sc_cookieName = "todo"
                               , sc_sessionTTL = 604800 -- one week
                               }
  appCfg     <- defaultSpockCfg Nothing (PCPool pool) ()
  let appCfg' = appCfg { spc_maxRequestSize = Just (5 * 1024 * 1024)
                       , spc_sessionCfg = sessionCfg'
                       }
  port <- maybe 3000 read <$> lookupEnv "PORT"
  runSpock port $ spock appCfg' app

app :: SpockM SqlBackend (Maybe (Int64, User)) state ()
app = do
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
      requireAuth onfail $ \_ -> do
      r <- runQuery' $ removePost pid
      json r

    get "posts" $
      requireAuth onfail $ \_ ->
      runQuery' getPosts >>= json

    get ("posts" <//> var) $ \pid ->
      requireAuth onfail $ \_ -> do
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
      sess <- readSession
      runQuery $ \conn ->
        liftIO (runReaderT action (Env conn sess))
