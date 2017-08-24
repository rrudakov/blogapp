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
  } deriving (Eq, Show)

instance ToJSON User where
  toJSON (User userid login password) =
    object ["id" .= userid, "username" .= login]

  toEncoding (User userid login password) =
    pairs ("id" .= userid <> "username" .= login)

instance FromJSON User where
  parseJSON = withObject "User" $ \ v -> User
    <$> v .:? "id"
    <*> v .: "username"
    <*> v .: "password"

instance ToRow User where
  toRow user = [ toField $ userId user
               , toField $ userLogin user
               , toField $ userPassword user ]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

lookupUser :: Connection -> String -> String -> IO (Either String User)
lookupUser conn username password = do
  res <- query conn "SELECT * FROM users WHERE username=?" [username]
  case res of
    [user] -> do
      case validatePassword (pack $ userPassword user) (pack password) of
        True -> return $ Right user
        False -> return $ Left wrongPasswordErr
    _ -> return $ Left loginIncorrectErr

allUsers :: Connection -> IO [User]
allUsers conn = query_ conn "SELECT * FROM users ORDER BY username"


saveUser :: Connection -> User -> IO (Either String Int)
saveUser conn user = handleJust constraintViolation handler $ do
  [Only i] <- query conn "INSERT INTO users (username, password) VALUES (?, ?) RETURNING id" (userLogin user, userPassword user)
  return $ Right i
  where
    handler (UniqueViolation "users_username_key") = return $ Left loginExistErr
    handler _ = return $ Left dataBaseErr

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
