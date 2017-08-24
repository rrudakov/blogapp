{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Server
  ( server
  , api
  ) where

import App.Errors
import App.Models
import App.Routes
import Control.Monad.IO.Class
import Data.Aeson
import Database.PostgreSQL.Simple (Connection)
import Servant
import Network.Wai (Application, Request)

data ErrJson = ErrJson String

instance ToJSON ErrJson where
  toJSON (ErrJson msg) = object ["err" .= msg]

  toEncoding (ErrJson msg) = pairs ("err" .= msg)

api :: Proxy API
api = Proxy

usersServer :: Connection -> Server UsersAPI
usersServer conn =
  allUsersHandler conn :<|>
  getUserHandler conn :<|>
  saveUserHandler conn :<|>
  updateUserHandler conn :<|>
  deleteUserHandler conn

server :: Connection -> Server API
server conn = usersServer conn

-- |User handlers
allUsersHandler :: Connection -> Handler [User]
allUsersHandler conn = liftIO $ allUsers conn

getUserHandler :: Connection -> Int -> Handler User
getUserHandler conn userid = do
  res <- liftIO $ getUser conn userid
  case res of
    Just user -> return user
    Nothing -> throwError $ err404 { errBody = encode $ ErrJson userNotFoundErr }

saveUserHandler :: Connection -> User -> Handler (Headers '[Header "Location" String] Int)
saveUserHandler conn user = do
  res <- liftIO $ saveUser conn user
  case res of
    Right userid -> return $ addHeader ("/users/" ++ show userid) userid
    Left err -> throwError $ err500 { errBody = encode $ ErrJson err }

updateUserHandler :: Connection -> Int -> User -> Handler ()
updateUserHandler conn userid user = do
  res <- liftIO $ updateUser conn userid user
  case res of
    1 -> return ()
    0 -> throwError $ err404 { errBody = encode $ ErrJson userNotFoundErr }
    _ -> throwError $ err500 { errBody = encode $ ErrJson serverErr }

deleteUserHandler :: Connection -> Int -> Handler ()
deleteUserHandler conn userid = do
  res <- liftIO $ deleteUser conn userid
  case res of
    1 -> return ()
    0 -> throwError $ err404 { errBody = encode $ ErrJson userNotFoundErr }
    _ -> throwError $ err500 { errBody = encode $ ErrJson serverErr }
