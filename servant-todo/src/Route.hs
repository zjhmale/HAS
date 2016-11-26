{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Route where

import Control.Monad.Reader         (ReaderT, runReaderT, lift)
import Data.Text as T hiding (map)
import           Control.Monad.Trans.Except
import           Data.Proxy
import           Servant hiding (Handler)
import           Data.Aeson
import           GHC.Generics         (Generic)
import           Data.Int             (Int64)
import           Model hiding (Post)
import Config

type API
  = "welcome" :> Get '[JSON] Value
  -- :<|> "posts" :> Get '[JSON] Value
  -- :<|> "posts" :> Capture "id" Int64 :> Get '[JSON] Value
  -- :<|> "posts" :> ReqBody '[JSON] PostView :> Post '[JSON] APIResult
  -- :<|> "posts" :> Capture "id" Int64 :> ReqBody '[JSON] PostView :> Put '[JSON] APIResult
  -- :<|> "posts" :> Capture "id" Int64 :> Delete '[JSON] APIResult

api :: Proxy API
api = Proxy

-- customize handler type, add a reader monad stack.
type Handler = ReaderT Config (ExceptT ServantErr IO)

data APIResult = APIResult {
    ok     :: Bool
  , output :: String
  } deriving Generic

instance ToJSON APIResult

data PostView = PostView
  { title   :: Text
  , content :: Text
  } deriving Generic

instance FromJSON PostView

app :: Config -> Application
app cfg = serve api (readerServer cfg)

readerServer :: Config -> Server API
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: Config -> Handler :~> ExceptT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

welcomeHandler :: Handler Value
welcomeHandler = return $ object ["msg" .= ("welcome" :: String)]

server :: ServerT API Handler
server
  = welcomeHandler
