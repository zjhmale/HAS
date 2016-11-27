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
, allPostIdTitles
, getPostById
, updatePost
, insertPost
, deletePost
, fromSqlKey
, toSqlKey
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

allPostIdTitles :: Handler [(Int64, Text)]
allPostIdTitles = do
  posts <- runDb $ selectList [] [Asc PostId]
  let pairs = map (\(Entity key Post{..}) -> (fromSqlKey key, postTitle)) posts
  return pairs
  {-
  runDb $ do
  idTitles <- select $
    from $ \post -> do
      orderBy [asc (post ^. PostId)]
      return (post ^. PostId, post ^. PostTitle)
  return $ ((fromSqlKey . unValue) *** unValue) <$> idTitles
  -}

getPostById :: Int64 -> Handler (Maybe Post)
getPostById postId = do
  posts <- runDb $ selectList [PostId ==. toSqlKey postId] []
  let posts' = map (\(Entity _ y) -> y) posts
  case posts' of
    []       -> return Nothing
    (post:_) -> return . Just $ post

insertPost :: Post -> Handler Int64
insertPost post = fromSqlKey <$> runDb (insert post)

updatePost :: Int64 -> Post -> Handler ()
updatePost pid Post{..} = do
  runDb $ updateWhere [PostId ==. toSqlKey pid] [ PostTitle   =. postTitle
                                                , PostContent =. postContent
                                                ]
  return ()

deletePost :: Int64 -> Handler ()
deletePost pid = do
  runDb $ deleteWhere [PostId ==. toSqlKey pid]
  return ()
