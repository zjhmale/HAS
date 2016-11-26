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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

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

import           Servant hiding (Handler, Post)
import Control.Monad.Trans.Except (throwE)
import           Config
--import           Control.Arrow
import           Control.Monad.Reader
--import           Data.Int             (Int64)
import           Data.Text            (Text)
import           Data.Time
import           Database.Persist.TH
import           Data.Int             (Int64)
import Database.Persist.MySQL (insert, deleteWhere, selectList, Entity(..), runMigration, runSqlPool, updateWhere, toSqlKey, SelectOpt(..), fromSqlKey, (==.), (=.), SqlBackend, SqlPersistT)

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

allPostIdTitles :: Handler [Post]
allPostIdTitles = do
  posts <- runDb $ selectList [] [Asc PostId]
  let posts' = map (\(Entity _ y) -> y) posts
  return posts'
  {-
  runDb $ do
  idTitles <- select $
    from $ \post -> do
      orderBy [asc (post ^. PostId)]
      return (post ^. PostId, post ^. PostTitle)
  return $ ((fromSqlKey . unValue) *** unValue) <$> idTitles
  -}

getPostById :: Int64 -> Handler Post
getPostById postId = do
  posts <- runDb $ selectList [PostId ==. toSqlKey postId] []
  let posts' = map (\(Entity _ y) -> y) posts
  case posts' of
    []       -> lift $ throwE err404
    (post:_) -> return post

insertPost :: Post -> Handler Int64
insertPost post = fromSqlKey <$> runDb (insert post)

editPost :: Int64 -> Post -> Handler ()
editPost pid Post{..} = do
  runDb $ updateWhere [PostId ==. toSqlKey pid] [ PostTitle   =. postTitle
                                                , PostContent =. postContent
                                                ]
  return ()

removePost :: Int64 -> Handler ()
removePost pid = do
  runDb $ deleteWhere [PostId ==. toSqlKey pid]
  return ()
