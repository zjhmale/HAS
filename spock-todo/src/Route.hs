{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Route where

import           Control.Monad.Reader
import           Controller
import           Data.Aeson                           (object, (.=))
import           Data.Int                             (Int64)
import           Model
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           System.Environment                   (lookupEnv)
import           Web.Spock
import           Web.Spock.Config

run :: IO ()
run = do
  port <- maybe 3000 read <$> lookupEnv "PORT"
  appCfg <- getAppCfg
  runSpock port $ spockApp appCfg

spockApp :: SpockCfg SqlBackend (Maybe (Int64, User)) state
         -> IO Middleware
spockApp cfg = spock cfg app

app :: SpockM SqlBackend (Maybe (Int64, User)) state ()
app = do
    middleware logStdoutDev
    let onfail = json $ object ["ok" .= False, "err" .= ("Session expired." :: String)]

    get root $ let msg = "welcome" :: String in json $ object ["msg" .= msg]

    post "posts" $ do
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
      sess <- readSession
      runQuery $ \conn ->
        liftIO (runReaderT action (Env conn sess))
