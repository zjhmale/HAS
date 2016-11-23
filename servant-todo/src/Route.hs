{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE TypeOperators #-}

module Route where

import           Control.Monad.Trans.Except
import           Data.Proxy
import           Servant
import           Data.Aeson
import           GHC.Generics         (Generic)

type API
    -- GET /things
    = "things" :> Get '[JSON] [Thing]
    -- GET /things/:id
    :<|> "things" :> Capture "id" Integer :> Get '[JSON] Thing

api :: Proxy API
api = Proxy

-- This is Servant's default handler type uses ExceptT.
-- type Handler a = EitherT ServantErr IO a

data Thing = Thing Integer
           deriving (Generic)

instance ToJSON Thing

getThingsFromDB :: Handler [Thing]
getThingsFromDB = return $ map Thing [1..3]

getThingFromDB :: Integer -> Handler (Maybe Thing)
getThingFromDB = return . Just . Thing

getThings :: Handler [Thing]
getThings = do
    things <- getThingsFromDB
    return things

getThing :: Integer -> Handler Thing
getThing thingID = do
    maybeThing <- getThingFromDB thingID
    case maybeThing of
        Just thing -> return thing
        Nothing -> throwE err404

server :: Server API
server
    -- GET /things
    = getThings
    -- GET /things/:id
    :<|> getThing

app :: Application
app = serve api server
