{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Models
  ( User (..)
  , AuthUserData (..)
  , lookupAuth
  , allUsers
  , saveUser
  , getUser
  , updateUser
  , deleteUser
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

-- |AuthUserData
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
  { userId :: !Int
  , userLogin :: !String
  , userPassword :: !String
  , userCreateAt :: UTCTime
  , userIsActive :: !Bool
  , userIsAdmin :: !Bool
  } deriving (Eq, Show)

instance ToJSON User where
  toJSON (User userid login _ created active admin) =
    object [ "id" .= userid
           , "username" .= login
           , "created" .= created
           , "is_active" .= active
           , "is_admin" .= admin
           ]

  toEncoding (User userid login _ created active admin) =
    pairs ( "id" .= userid <>
            "username" .= login <>
            "created" .= created <>
            "is_active" .= active <>
            "is_admin" .= admin )

instance FromJSON User where
  parseJSON = withObject "User" $ \ v -> User
    <$> v .: "id"
    <*> v .: "username"
    <*> v .: "password"
    <*> v .: "created"
    <*> v .:? "is_active" .!= True
    <*> v .:? "is_admin" .!= False

instance ToRow User where
  toRow user = [ toField $ userId user
               , toField $ userLogin user
               , toField $ userPassword user ]

instance FromRow User where
  fromRow = User
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

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
  res <- query conn "SELECT * FROM users WHERE id=?" [userid :: Int]
  case res of
    [user] -> return $ Just user
    _ -> return Nothing

updateUser :: Connection -> Int -> User -> IO (Either BlogError ())
updateUser conn userid user = do
  h <- hashPasswordUsingPolicy fastBcryptHashingPolicy (pack $ userPassword user)
  case h of
    Just hash -> do
      res <- execute conn "UPDATE users SET password=? WHERE id=?" (hash, userid)
      case res of
        1 -> return $ Right ()
        0 -> return $ Left UserNotFoundErr
        _ -> return $ Left ServerErr
    Nothing -> return $ Left ServerErr

deleteUser :: Connection -> Int -> IO Int
deleteUser conn userid = execute conn "DELETE FROM users WHERE id=?" [userid] >>= \res ->  return $ fromIntegral res
