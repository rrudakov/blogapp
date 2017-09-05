{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Models
  ( -- * Types
    User (..)
  , AuthUserData (..)
  , BlogPost (..)
    -- * Users functions
  , lookupAuth
  , allUsers
  , saveUser
  , getUser
  , updateUser
  , deleteUser
    -- * Posts functions
  , allPosts
  , allUserPosts
  , getPost
  , createPost
  , updatePost
  , deletePost
  ) where

import App.Errors
import Control.Exception.Base
import Crypto.BCrypt
import Data.Aeson
import Data.ByteString.Char8 (pack)
import Data.Monoid
import Data.Serialize (Serialize)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Generics
import Servant.Server.Experimental.Auth.Cookie (AuthCookieData)

-- |Data type used for authentication
data AuthUserData = AuthUserData
  { authId :: Maybe Int
  , authLogin :: !String
  , authPassword :: !String
  } deriving (Eq, Show, Generic)

instance Serialize AuthUserData

type instance AuthCookieData = AuthUserData

instance FromJSON AuthUserData where
  parseJSON = withObject "AuthUserData" $ \ v -> AuthUserData
    <$> v .:? "id"
    <*> v .: "username"
    <*> v .: "password"

lookupAuth :: Connection -> AuthUserData -> IO (Either BlogError User)
lookupAuth conn auth = do
  res <- query conn "SELECT * FROM users WHERE username=?" [authLogin auth]
  case res of
    [user] -> do
      case validatePassword (pack $ userPassword user) (pack $  authPassword auth) of
        True -> return $ Right user
        False -> return $ Left WrongPasswordErr
    _ -> return $ Left LoginIncorrectErr

-- |User data type
data User = User
  { userId        :: !Int
  , userLogin     :: !String
  , userPassword  :: !String
  , userCreatedAt :: UTCTime
  , userUpdatedAt :: Maybe UTCTime
  , userIsActive  :: !Bool
  , userIsAdmin   :: !Bool
  } deriving (Eq, Show)

instance ToJSON User where
  toJSON (User userid login _ created updated active admin) =
    object [ "id" .= userid
           , "username" .= login
           , "created" .= created
           , "updated" .= updated
           , "is_active" .= active
           , "is_admin" .= admin
           ]

  toEncoding (User userid login _ created updated active admin) =
    pairs ( "id" .= userid <>
            "username" .= login <>
            "created" .= created <>
            "updated" .= updated <>
            "is_active" .= active <>
            "is_admin" .= admin )

instance FromJSON User where
  parseJSON = withObject "User" $ \ v -> User
    <$> v .: "id"
    <*> v .: "username"
    <*> v .: "password"
    <*> v .: "created"
    <*> v .:? "updated"
    <*> v .:? "is_active" .!= True
    <*> v .:? "is_admin" .!= False

instance ToRow User where
  toRow user = [ toField $ userId user
               , toField $ userLogin user
               , toField $ userPassword user ]

instance FromRow User where
  fromRow = User
    <$> field -- ^ userId
    <*> field -- ^ userLogin
    <*> field -- ^ userPassword
    <*> field -- ^ userCreateAt
    <*> field -- ^ userUpdatedAt
    <*> field -- ^ userIsActive
    <*> field -- ^ userIsAdmin

allUsers :: Connection -> IO [User]
allUsers conn = query_ conn "SELECT * FROM users ORDER BY username"

saveUser :: Connection -> AuthUserData -> IO (Either BlogError Int)
saveUser conn auth = handleJust constraintViolation handler $ do
  h <- hashPasswordUsingPolicy fastBcryptHashingPolicy (pack $ authPassword auth)
  case h of
    Just hash -> do
      [Only i] <- query conn "INSERT INTO users (username, password, created_at, is_active, is_admin) VALUES (?, ?, now(), ?, ?) RETURNING id" (authLogin auth, hash, True, False)
      return $ Right i
    Nothing -> return $ Left ServerErr
  where
    handler (UniqueViolation "users_username_key") = return $ Left LoginExistErr
    handler _ = return $ Left DataBaseErr

getUser :: Connection -> Int -> IO (Maybe User)
getUser conn userid = do
  res <- query conn "SELECT * FROM users WHERE id=?" [userid]
  case res of
    [user] -> return $ Just user
    _ -> return Nothing

updateUser :: Connection -> Int -> User -> IO (Either BlogError ())
updateUser conn userid user = do
  h <- hashPasswordUsingPolicy fastBcryptHashingPolicy (pack $ userPassword user)
  case h of
    Just hash -> do
      res <- execute conn "UPDATE users SET password=?, updated_at=now() WHERE id=?" (hash, userid)
      case res of
        1 -> return $ Right ()
        0 -> return $ Left UserNotFoundErr
        _ -> return $ Left ServerErr
    Nothing -> return $ Left ServerErr

deleteUser :: Connection -> Int -> IO Int
deleteUser conn userid =
  execute conn "DELETE FROM users WHERE id=?" [userid] >>= \res ->
    return $ fromIntegral res

-- |Post data type
data BlogPost = BlogPost
  { postId        :: Maybe Int
  , postAuthorId  :: Int
  , postCreatedAt :: Maybe UTCTime
  , postUpdatedAt :: Maybe UTCTime
  , postTitle     :: String
  , postContent   :: String
  } deriving (Eq, Show)

instance ToJSON BlogPost where
  toJSON (BlogPost i author created updated title content) =
    object [ "id" .= i
           , "author" .= author
           , "created" .= created
           , "updated" .= updated
           , "title" .= title
           , "content" .= content ]

  toEncoding (BlogPost i author created updated title content) =
    pairs ( "id" .= i <>
            "author" .= author <>
            "created" .= created <>
            "updated" .= updated <>
            "title" .= title <>
            "content" .= content )

instance FromJSON BlogPost where
  parseJSON = withObject "BlogPost" $ \ v -> BlogPost
    <$> v .:? "id"
    <*> v .: "author"
    <*> v .:? "created"
    <*> v .:? "updated"
    <*> v .: "title"
    <*> v .: "content"

instance ToRow BlogPost where
  toRow (BlogPost i author created updated title content) =
    [ toField i
    , toField author
    , toField created
    , toField updated
    , toField title
    , toField content ]

instance FromRow BlogPost where
  fromRow = BlogPost
    <$> field -- ^ postId
    <*> field -- ^ postAuthorId
    <*> field -- ^ postCreatedAt
    <*> field -- ^ postUpdatedAt
    <*> field -- ^ postTitle
    <*> field -- ^ postContent

allPosts :: Connection -> IO [BlogPost]
allPosts conn = query_ conn "SELECT * FROM posts ORDER BY created_at DESC"

allUserPosts :: Connection -> Int -> IO [BlogPost]
allUserPosts conn author_id = query conn "SELECT * FROM posts WHERE author=? ORDER BY created_at DESC" [author_id]

getPost :: Connection -> Int -> IO (Maybe BlogPost)
getPost conn post_id = do
  res <- query conn "SELECT * FROM posts WHERE id=?" [post_id]
  case res of
    [post] -> return $ Just post
    _ -> return Nothing

createPost :: Connection -> Int -> BlogPost -> IO Int
createPost conn uid post = do
  [Only i] <- query conn "INSERT INTO posts (author, created_at, title, content) VALUES (?, now(), ?, ?) RETURNING id" (uid, postTitle post, postContent post)
  return i

updatePost :: Connection -> Int -> Int -> BlogPost -> IO (Either BlogError ())
updatePost conn uid pid post = do
  res <- execute conn "UPDATE posts SET updated_at=now(), title=?, content=? WHERE id=? AND author=?" (postTitle post, postContent post, pid, uid)
  case res of
    1 -> return $ Right ()
    0 -> return $ Left PostNotFoundErr
    _ -> return $ Left ServerErr

deletePost :: Connection -> Int -> Int -> IO Int
deletePost conn uid pid =
  execute conn "DELETE FROM posts WHERE id=? AND author=?" (pid, uid) >>= \res ->
    return $ fromIntegral res
