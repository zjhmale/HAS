{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model(
  Query
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
, migrateAll
, User(..)
, Post(..)
) where

import           Config
import           Control.Arrow
import           Control.Monad.Reader
import           Data.Int             (Int64)
import           Data.Text            (Text)
import           Data.Time
import           Database.Esqueleto
import           Database.Persist.TH

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

type Query a = ReaderT Config IO a

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
