{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

module Controller where

import           Control.Monad.Reader
import           Crypto.PasswordStore
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Int             (Int64)
import           Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import           Data.Time
import           GHC.Generics         (Generic)
import           Model

loginSignupPost :: Text -> Text -> Text -> Query (Value, Maybe (Int64, User))
loginSignupPost username password _type = do
  maybeUser <- getUserByUsername username
  (ok', err', sess) <-
    if T.length username < 6 || T.length password < 6
       then return (False, "Length of username and password should be at least 6.", Nothing)
       else
         case _type of
           "login" ->
             case maybeUser of
               Nothing -> return (False, "User not found.", Nothing)
               Just userWithId@(_, user) ->
                 if verifyPassword (encodeUtf8 password) $ encodeUtf8 (userPassword user)
                   then return (True, "success", Just userWithId)
                   else return (False, "Wrong password.", Nothing)
           "signup" ->
             case maybeUser of
               Just _ -> return (False, "Username is already used.", Nothing)
               Nothing -> do
                 salt <- liftIO genSaltIO
                 let hashPassword =  decodeUtf8 $ makePasswordSalt (encodeUtf8 password) salt 17
                 userid <- addUser username hashPassword
                 insertedUser <- getUserByUsername username
                 return (True, show userid, insertedUser)
           _ -> return (False, "?", Nothing)
  return (object ["ok" .= ok', "err" .= err'], sess)

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
    Just orig@Post{..} ->
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
