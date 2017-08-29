{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module App.Server
  ( server
  , authContext
  ) where

import App.Errors
import App.Models
import Control.Monad.Catch (catch)
import Data.ByteString.Char8 (pack, unpack)
import App.Routes (PublicAPI, UsersAPI, API) 
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default (def)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Network.Wai (Application, Request)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Servant.Server.Experimental.Auth.Cookie

authHandler :: (ServerKeySet s)
  => s
  -> AuthHandler Request (WithMetadata AuthUserData)
authHandler sks = mkAuthHandler $ \ request ->
  (getSession authSettings sks request) `catch` handleEx >>= maybe
    (throwError err403 { errBody = "No cookies" })
    (return)
  where
    handleEx :: AuthCookieException -> Handler (Maybe (WithMetadata AuthUserData))
    handleEx ex = throwError err403 { errBody = "JOPA" }

authSettings :: AuthCookieSettings
authSettings = def
  { acsCookieFlags = ["HttpOnly"] }

authContext :: (ServerKeySet s)
  => s
  -> Context (AuthHandler Request (WithMetadata AuthUserData) ': '[])
authContext sks = ((authHandler sks) :. EmptyContext)

-- |Public API server
publicServer :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> Server PublicAPI
publicServer rs sks conn =
  saveUserHandler conn :<|>
  loginUserHandler rs sks conn

-- |Users API server
usersServer :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> Server UsersAPI
usersServer rs sks conn =
  allUsersHandler rs sks conn :<|>
  getUserHandler rs sks conn :<|>
  updateUserHandler rs sks conn :<|>
  deleteUserHandler rs sks conn

server :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> Server API
server rs sks conn =
  publicServer rs sks conn :<|>
  usersServer rs sks conn

-- |User handlers
loginUserHandler :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> AuthUserData
  -> Handler (Cookied User)
loginUserHandler rs sks conn auth = do
  res <- liftIO $ lookupAuth conn auth
  case res of
    Right user -> addSession' auth user
    Left err -> throwError $ err401 { errBody = encode err }
  where
    addSession' = addSession authSettings rs sks

allUsersHandler :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> WithMetadata AuthUserData
  -> Handler (Cookied [User])
allUsersHandler rs sks conn =
  cookied authSettings rs sks (Proxy :: Proxy AuthUserData) $ allUsersHandler'
  where
    allUsersHandler' :: AuthUserData -> Handler [User]
    allUsersHandler' auth = liftIO $ allUsers conn >>= return

getUserHandler :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> Int
  -> WithMetadata AuthUserData
  -> Handler (Cookied User)
getUserHandler rs sks conn userid =
  cookied authSettings rs sks (Proxy :: Proxy AuthUserData) $ getUserHandler'
  where
    getUserHandler' :: AuthUserData -> Handler User
    getUserHandler' auth = do
      res <- liftIO $ getUser conn userid
      case res of
        Just user -> return user
        Nothing -> throwError $ err404 { errBody = encode UserNotFoundErr }

saveUserHandler :: Connection -> User -> Handler (Headers '[Header "Location" String] Int)
saveUserHandler conn user = do
  res <- liftIO $ saveUser conn user
  case res of
    Right userid -> return $ addHeader ("/users/" ++ show userid) userid
    Left err -> throwError $ err500 { errBody = encode err }

updateUserHandler :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> Int
  -> User
  -> WithMetadata AuthUserData
  -> Handler (Cookied ())
updateUserHandler rs sks conn userid user =
  cookied authSettings rs sks (Proxy :: Proxy AuthUserData) $ updateUserHandler'
  where
    updateUserHandler' :: AuthUserData -> Handler ()
    updateUserHandler' auth = do
      res <- liftIO $ updateUser conn userid user
      case res of
        1 -> return ()
        0 -> throwError $ err404 { errBody = encode UserNotFoundErr }
        _ -> throwError $ err500 { errBody = encode ServerErr }

deleteUserHandler :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> Int
  -> WithMetadata AuthUserData
  -> Handler (Cookied ())
deleteUserHandler rs sks conn userid =
  cookied authSettings rs sks (Proxy :: Proxy AuthUserData) $ deleteUserHandler'
  where
    deleteUserHandler' :: AuthUserData -> Handler ()
    deleteUserHandler' auth = do
      res <- liftIO $ deleteUser conn userid
      case res of
        1 -> return ()
        0 -> throwError $ err404 { errBody = encode UserNotFoundErr }
        _ -> throwError $ err500 { errBody = encode ServerErr }
