{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Route where

import           Config
import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.Trans.Except
import           Controller
import           Data.Aeson
import           Data.Int                   (Int64)
import           Data.Proxy
import           Servant                    hiding (Handler)

type PostAPI
  = Get '[JSON] Value
  :<|> Capture "id" Int64 :> Get '[JSON] Value
  :<|> ReqBody '[JSON] PostView :> Post '[JSON] Value
  :<|> Capture "id" Int64 :> ReqBody '[JSON] PostView :> Put '[JSON] Value
  :<|> Capture "id" Int64 :> Delete '[JSON] Value

type API
  = "welcome" :> Get '[JSON] Value
  :<|> "posts" :> PostAPI

api :: Proxy API
api = Proxy

app :: Config -> Application
app cfg = serve api (readerServer cfg)

readerServer :: Config -> Server API
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: Config -> Handler :~> ExceptT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

server :: ServerT API Handler
server
  = welcome
  :<|> getPosts
  :<|> getPost
  :<|> createPost
  :<|> editPost
  :<|> removePost
