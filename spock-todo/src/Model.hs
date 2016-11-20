{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}

module Model(
  runConnPool
, Query
, allPostIdTitles
, getPostById
, updatePost
, insertPost
, deletePost
, addUser
, getUserByUsername
, SqlBackend
, fromSqlKey
, toSqlKey
, User(..)
, Post(..)
, Env(..)) where

import Data.Text (Text)
import Data.Int (Int64)
import Control.Arrow
import Database.Persist.TH
import Database.Persist.MySQL hiding ((==.), (=.), update, delete)
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Database.Esqueleto
import Data.Time
import GHC.Generics (Generic)
import Data.Aeson

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name Text
    password Text
    deriving Show
Post json
    authorId UserId
    title Text
    content Text
    createdAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
|]

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo
  { connectPort = 3306
  , connectHost = "127.0.0.1"
  , connectUser = "root"
  , connectPassword = "cleantha"
  , connectDatabase = "todo"
  }

runConnPool :: (ConnectionPool -> IO ()) -> IO ()
runConnPool action =
  runResourceT . runStdoutLoggingT $ withMySQLPool connectionInfo 10 $ \pool -> liftIO $ do
    runSqlPersistMPool (runMigration migrateAll) pool
    action pool

data Env = Env {
    sqlHandler :: SqlBackend
  , currUser :: Maybe (Int64, User)
  }

type Query a = ReaderT Env IO a

runDb :: SqlPersistM a -> Query a
runDb query = asks sqlHandler >>= lift . runSqlPersistM query

allPostIdTitles :: Query [(Int64, Text)]
allPostIdTitles = runDb $ do
  idTitles <- select $
    from $ \post -> do
      orderBy [asc (post ^. PostId)]
      return (post ^. PostId, post ^. PostTitle)
  return $ ((fromSqlKey . unValue) *** unValue) <$> idTitles

getPostById :: Int64 -> Query (Maybe Post)
getPostById postId = runDb $ do
  posts <- select $
    from $ \post -> do
      where_ (post ^. PostId ==. valkey postId)
      limit 1
      return post
  case posts of
    [post] -> return . Just . entityVal $ post
    _ -> return Nothing

updatePost :: Int64 -> Post -> Query ()
updatePost pid Post{..} = runDb $
  update $ \post -> do
    set post [ PostTitle        =. val postTitle
             , PostContent      =. val postContent
             ]
    where_ (post ^. PostId ==. valkey pid)

insertPost :: Post -> Query Int64
insertPost post = fromSqlKey <$> runDb (insert post)

deletePost :: Int64 -> Query ()
deletePost pid = runDb $
  delete $
  from $ \post ->
  where_ (post ^. PostId ==. valkey pid)

getUserByUsername :: Text -> Query (Maybe (Int64, User))
getUserByUsername username = runDb $ do
  users <- select $
    from $ \user -> do
      where_ (user ^. UserName ==. val username)
      limit 1
      return user
  case users of
    [user] -> return . Just $
      ((fromSqlKey . entityKey) &&& entityVal) user
    _ -> return Nothing

addUser :: Text -> Text -> Query (Key User)
addUser username hashPassword =
  runDb . insert $ User username hashPassword
