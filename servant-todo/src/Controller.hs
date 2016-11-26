{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

module Controller where

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
createPost pv = return $ apiResult True (0 :: Int64)

updatePost :: Int64 -> PostView -> Handler Value
updatePost id pv = return $ apiResult True id

deletePost :: Int64 -> Handler Value
deletePost id = return $ apiResult True id

{-
newPost :: Int64 -> PostView -> Query APIResult
newPost uid (PostView title content) = do
  t <- liftIO getCurrentTime
  let post = Post { postAuthorId = toSqlKey uid
                  , postTitle = title
                  , postContent = content
                  , postCreatedAt = addUTCTime (8 * 3600) t -- UTC+8
                  }
  newid <- insertPost post
  return APIResult {ok=True, output="New post #" ++ show newid ++ " created."}

editPost :: Int64 -> Int64 -> PostView -> Query APIResult
editPost pid uid (PostView title content) = do
  maybeOrig <- getPostById pid
  case maybeOrig of
    Nothing -> return APIResult {ok=False, output="Post not found."}
    Just orig@Post{..} ->
      if fromSqlKey postAuthorId == uid
        then do
          let post = orig { postTitle = title
                          , postContent = content
                          }
          updatePost pid post
          return APIResult {ok=True, output="Post updated."}
        else return APIResult {ok=False, output="Not authorized."}

removePost :: Int64 -> Int64 -> Query APIResult
removePost pid uid = do
  maybeOrig <- getPostById pid
  case maybeOrig of
    Nothing -> return APIResult {ok=False, output="Post not found."}
    Just Post{..} ->
      if fromSqlKey postAuthorId == uid
      then do
        deletePost pid
        return APIResult {ok=True, output="Post deleted."}
      else return APIResult {ok=False, output="Not authorized."}

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
