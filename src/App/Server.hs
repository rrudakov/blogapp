{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module App.Server
  ( -- * Functions
    server
  , authContext
  ) where

import           App.Errors
import           App.Models
import           App.Routes (PublicAPI, UsersAPI, PostsAPI, API)
import           Control.Monad.Catch (catch)
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString as BS
import           Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Database.PostgreSQL.Simple (Connection)
import           Network.Wai (Application, Request)
import           Network.Wai (requestHeaders)
import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import qualified Web.JWT as JWT

-- |JSON Web Token authorization staff
authHandler :: JWT.Secret -> Connection -> AuthHandler Request User
authHandler key conn = mkAuthHandler $ \ req -> do
  case lookup "Authorization" (requestHeaders req) of
    Nothing -> throw401 AuthRequiredErr
    Just token ->
      case decodeToken key token of
        Nothing -> throw401 InvalidTokenErr
        Just creds -> do
          res <- liftIO $ lookupAuth conn creds
          case res of
            Left err -> throw401 err
            Right user -> return user

authContext :: JWT.Secret -> Connection -> Context (AuthHandler Request User ': '[])
authContext key conn = authHandler key conn :. EmptyContext

decodeToken :: JWT.Secret -> BS.ByteString -> Maybe UserCredentials
decodeToken key token = case creds of
  Just (Success c) -> Just c
  Just (Error s) -> error s
  Nothing -> Nothing
  where
    t' = decodeUtf8 token
    (_, t) = T.splitAt (T.length "Token ") t'
    mJwt = JWT.decodeAndVerifySignature key t
    json = JWT.unregisteredClaims . JWT.claims <$> mJwt >>= Map.lookup "credentials"
    creds = fromJSON <$> json

encodeToken :: JWT.Secret -> UserCredentials -> JWT.JSON
encodeToken key creds =
  JWT.encodeSigned
    JWT.HS256
    key
    JWT.def
    {JWT.unregisteredClaims = Map.singleton "credentials" $ toJSON creds}

-- |Public API server
publicServer :: JWT.Secret -> Connection -> Server PublicAPI
publicServer key conn =
  saveUserHandler key conn :<|>
  loginUserHandler key conn

-- |Users API server
usersServer :: Connection -> User -> Server UsersAPI
usersServer conn user =
  allUsersHandler conn user :<|>
  getUserHandler conn user :<|>
  updateUserHandler conn user :<|>
  deleteUserHandler conn user

-- |Posts API server
postsServer :: Connection -> User -> Server PostsAPI
postsServer conn user uid =
  allUserPostsHandler conn user uid :<|>
  getUserPostHandler conn user uid :<|>
  createUserPostHandler conn user uid :<|>
  updateUserPostHandler conn user uid :<|>
  deleteUserPostHandler conn user uid

server :: JWT.Secret -> Connection -> Server API
server key conn =
  publicServer key conn :<|>
  usersServer conn :<|>
  postsServer conn

-- |Authorization/Registration handlers
loginUserHandler :: JWT.Secret -> Connection -> UserCredentials -> Handler UserWithToken
loginUserHandler key conn creds = do
  res <- liftIO $ lookupAuth conn creds
  case res of
    Right user -> return $ UserWithToken (authLogin creds) (T.unpack $ encodeToken key creds)
    Left err -> throwError $ err401 {errBody = encode err}

-- |User handlers
allUsersHandler :: Connection -> User -> Handler [User]
allUsersHandler conn user = do
  res <- liftIO . withAdminPermissions user conn $ allUsers conn
  case res of
    Right users -> return users
    Left err ->
      case err of
        WrongAuthCookiesErr -> throw401 err
        AccessDeniedErr -> throw403 err

getUserHandler :: Connection -> User -> Int -> Handler User
getUserHandler conn user userid = do
      res <- liftIO $ getUser conn userid
      case res of
        Just user -> return user
        Nothing -> throw404 UserNotFoundErr

saveUserHandler :: JWT.Secret -> Connection -> UserCredentials -> Handler UserWithToken
saveUserHandler key conn creds = do
  res <- liftIO $ saveUser conn creds
  case res of
    Right userid -> return $ UserWithToken (authLogin creds) (T.unpack $ encodeToken key creds)
    Left err -> throw409 err

updateUserHandler :: Connection -> User -> Int -> User -> Handler ()
updateUserHandler conn user uid u = do
  res <- liftIO . withOwnRestriction user conn uid $ updateUser conn uid u
  case res of
    Right r -> case r of
      Right () -> return ()
      Left err -> case err of
        UserNotFoundErr -> throw404 err
        ServerErr -> throw500 err
    Left err -> case err of
      WrongAuthCookiesErr -> throw401 err
      AccessDeniedErr -> throw403 err

deleteUserHandler :: Connection -> User -> Int -> Handler ()
deleteUserHandler conn user uid = do
  res <- liftIO . withAdminPermissions user conn $ deleteUser conn uid
  case res of
    Right 1 -> return ()
    Right 0 -> throw404 UserNotFoundErr
    Right _ -> throw500 ServerErr
    Left err -> case err of
      WrongAuthCookiesErr -> throw401 err
      AccessDeniedErr -> throw403 err

-- |Posts handlers
allUserPostsHandler :: Connection -> User -> Int -> Handler [BlogPost]
allUserPostsHandler conn user uid = do
  res <- liftIO . withOwnRestriction user conn uid $ allUserPosts conn uid
  case res of
    Right posts -> return posts
    Left err ->
      case err of
        WrongAuthCookiesErr -> throw401 err
        AccessDeniedErr -> throw403 err

getUserPostHandler :: Connection -> User -> Int -> Int -> Handler BlogPost
getUserPostHandler conn user uid pid = do
  res <- liftIO . withOwnRestriction user conn uid $ getPost conn pid
  case res of
    Right post ->
      case post of
        Just p -> return p
        Nothing -> throw404 PostNotFoundErr
    Left err ->
      case err of
        WrongAuthCookiesErr -> throw401 err
        AccessDeniedErr -> throw403 err

createUserPostHandler :: Connection -> User -> Int -> BlogPost -> Handler Int
createUserPostHandler conn user uid post = do
  res <- liftIO . withOwnRestriction user conn uid $ createPost conn uid post
  case res of
    Right pid -> return pid
    Left err -> throw403 err

updateUserPostHandler :: Connection -> User -> Int -> Int -> BlogPost -> Handler ()
updateUserPostHandler conn user uid pid post = do
  res <- liftIO . withOwnRestriction user conn uid $ updatePost conn uid pid post
  case res of
    Right updated ->
      case updated of
        Right () -> return ()
        Left err ->
          case err of
            PostNotFoundErr -> throw404 err
            ServerErr -> throw500 err
    Left err -> throw403 err

deleteUserPostHandler :: Connection -> User -> Int -> Int -> Handler ()
deleteUserPostHandler conn user uid pid = do
  res <- liftIO . withOwnRestriction user conn uid $ deletePost conn uid pid
  case res of
    Right 1 -> return ()
    Right 0 -> throw404 PostNotFoundErr
    Right _ -> throw500 ServerErr
    Left err -> throw403 err

-- |Execute action only if authorized as admin
withAdminPermissions :: User -> Connection -> IO a -> IO (Either BlogError a)
withAdminPermissions user conn action = do
  if userIsAdmin user
    then do
    res <- action
    return $ Right res
    else return $ Left AccessDeniedErr

-- |Execute action only if owner or admin
withOwnRestriction :: User -> Connection -> Int -> IO a -> IO (Either BlogError a)
withOwnRestriction user conn aid action = do
  if userIsAdmin user || aid == userId user
    then do
    res <- action
    return $ Right res
    else return $ Left AccessDeniedErr
