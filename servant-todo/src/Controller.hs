{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

module Controller where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson
import           Data.Int             (Int64)
import           Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import           Data.Time
import           GHC.Generics         (Generic)
import           Model
import Config

data PostView = PostView
  { title   :: Text
  , content :: Text
  } deriving Generic

instance FromJSON PostView

welcome :: Handler Value
welcome = return $ object ["msg" .= ("welcome" :: String)]

apiResult :: ToJSON a => Bool -> a -> Value
apiResult status result = object ["ok" .= status, "output" .= result]

getAllPosts :: Handler Value
getAllPosts = return $ apiResult True ([] :: [Int64])

getPost :: Int64 -> Handler Value
getPost id = return $ apiResult True id

createPost :: PostView -> Handler Value
createPost PostView{..} = do
  t <- liftIO getCurrentTime
  let post = Post { postTitle = title
                  , postContent = content
                  , postCreatedAt = addUTCTime (8 * 3600) t -- UTC+8
                  }
  newid <- insertPost post
  return $ apiResult True newid

editPost :: Int64 -> PostView -> Handler Value
editPost pid PostView{..} = do
  maybeOrig <- getPostById pid
  case maybeOrig of
    Nothing -> return $ apiResult False ("Post not found" :: String)
    Just orig@Post{..} -> do
      let post = orig { postTitle = title
                      , postContent = content
                      }
      updatePost pid post
      return $ apiResult True ("Post updated" :: String)

removePost :: Int64 -> Handler Value
removePost pid = do
  maybeOrig <- getPostById pid
  case maybeOrig of
    Nothing -> return $ apiResult False ("Post not found" :: String)
    Just Post{..} -> do
      deletePost pid
      return $ apiResult True ("Post deleted." :: String)

{-
getPost :: Int64 -> Query Value
getPost pid = do
  post <- getPostById pid
  case post of
    Just p -> return $ object ["ok" .= True, "output" .= p]
    Nothing -> return $ object ["ok" .= False, "output" .= ("Post not found." :: String)]

getPosts :: Query Value
getPosts = do
  posts <- allPostIdTitles
  return $ object ["ok" .= True, "output" .= posts]
-}
