{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Route where

import Control.Monad.Reader         (ReaderT, runReaderT, lift)
import           Control.Monad.Trans.Except
import Data.Text as T hiding (map)
import           Data.Proxy
import           Servant hiding (Handler)
import           Data.Aeson
import           GHC.Generics         (Generic)
import           Data.Int             (Int64)
import           Model hiding (Post)
import Controller
import Config

type API
  = "welcome" :> Get '[JSON] Value
  :<|> "posts" :> Get '[JSON] Value
  :<|> "posts" :> Capture "id" Int64 :> Get '[JSON] Value
  :<|> "posts" :> ReqBody '[JSON] PostView :> Post '[JSON] Value
  :<|> "posts" :> Capture "id" Int64 :> ReqBody '[JSON] PostView :> Put '[JSON] Value
  :<|> "posts" :> Capture "id" Int64 :> Delete '[JSON] Value

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
  :<|> getAllPosts
  :<|> getPost
  :<|> createPost
  :<|> updatePost
  :<|> deletePost
