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
{-# LANGUAGE FlexibleContexts #-}

module Model(
  connectionInfo
--, allPostIdTitles
--, getPostById
--, updatePost
--, insertPost
--, deletePost
--, addUser
--, getUserByUsername
--, SqlBackend
--, fromSqlKey
--, toSqlKey
, doMigrations
, Post(..)
) where

import           Config
--import           Control.Arrow
import           Control.Monad.Reader
--import           Data.Int             (Int64)
import           Data.Text            (Text)
import           Data.Time
import           Database.Esqueleto
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Post json
    title Text
    content Text
    createdAt UTCTime default=CURRENT_TIMESTAMP
    deriving Show
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

{-
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
-}
