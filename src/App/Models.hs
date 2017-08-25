{-# LANGUAGE OverloadedStrings #-}
module App.Models
  ( User (..)
  , lookupUser
  , allUsers
  , saveUser
  , getUser
  , updateUser
  , deleteUser
  ) where

import App.Errors
import Control.Exception.Base
import Data.Aeson
import Data.Time.Clock.POSIX
import Crypto.BCrypt
import Data.ByteString.Char8 (pack)
import Data.Monoid
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow


-- |User data type
data User = User
  { userId :: Maybe Int
  , userLogin :: !String
  , userPassword :: !String
  , userIsActive :: !Bool
  , userIsAdmin :: !Bool
  } deriving (Eq, Show)

instance ToJSON User where
  toJSON (User userid login _ active admin) =
    object [ "id" .= userid
           , "username" .= login
           , "is_active" .= active
           , "is_admin" .= admin
           ]

  toEncoding (User userid login _ active admin) =
    pairs ( "id" .= userid <>
            "username" .= login <>
            "is_active" .= active <>
            "is_admin" .= admin )

instance FromJSON User where
  parseJSON = withObject "User" $ \ v -> User
    <$> v .:? "id"
    <*> v .: "username"
    <*> v .: "password"
    <*> v .:? "is_active" .!= True
    <*> v .:? "is_admin" .!= False

instance ToRow User where
  toRow user = [ toField $ userId user
               , toField $ userLogin user
               , toField $ userPassword user ]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field

lookupUser :: Connection -> String -> String -> IO (Either BlogError User)
lookupUser conn username password = do
  res <- query conn "SELECT * FROM users WHERE username=?" [username]
  case res of
    [user] -> do
      case validatePassword (pack $ userPassword user) (pack password) of
        True -> return $ Right user
        False -> return $ Left WrongPasswordErr
    _ -> return $ Left LoginIncorrectErr

allUsers :: Connection -> IO [User]
allUsers conn = query_ conn "SELECT * FROM users ORDER BY username"


saveUser :: Connection -> User -> IO (Either BlogError Int)
saveUser conn user = handleJust constraintViolation handler $ do
  h <- hashPasswordUsingPolicy fastBcryptHashingPolicy (pack $ userPassword user)
  case h of
    Just hash -> do
      [Only i] <- query conn "INSERT INTO users (username, password) VALUES (?, ?) RETURNING id" (userLogin user, hash)
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

updateUser :: Connection -> Int -> User -> IO Int
updateUser conn userid user = do
  res <- execute conn "UPDATE users SET password=? WHERE id=?" (userPassword user, userid)
  return $ fromIntegral res

deleteUser :: Connection -> Int -> IO Int
deleteUser conn userid = execute conn "DELETE FROM users WHERE id=?" [userid] >>= \res ->  return $ fromIntegral res


-- -- |Profile data type
-- data UserProfile = 
