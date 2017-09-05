{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module App.Server
  ( -- * Functions
    server
  , authContext
  ) where

import App.Errors
import App.Models
import App.Routes (PublicAPI, UsersAPI, PostsAPI, API)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Char8 (pack, unpack)
import Data.Default (def)
import Database.PostgreSQL.Simple (Connection)
import Network.Wai (Application, Request)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Servant.Server.Experimental.Auth.Cookie

-- |Authorization staff
authHandler :: (ServerKeySet s)
  => s
  -> AuthHandler Request (WithMetadata AuthUserData)
authHandler sks = mkAuthHandler $ \ request ->
  (getSession authSettings sks request) `catch` handleEx >>= maybe
    (throw403 NoAuthCookiesErr)
    (return)
  where
    handleEx :: AuthCookieException -> Handler (Maybe (WithMetadata AuthUserData))
    handleEx ex = throw403 WrongAuthCookiesErr

authSettings :: AuthCookieSettings
authSettings = def {acsCookieFlags = ["HttpOnly"]}

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
  loginUserHandler rs sks conn :<|>
  logoutUserHandler

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

-- |Posts API server
postsServer :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> Server PostsAPI
postsServer rs sks conn uid =
  allUserPostsHandler rs sks conn uid :<|>
  getUserPostHandler rs sks conn uid :<|>
  createUserPostHandler rs sks conn uid :<|>
  updateUserPostHandler rs sks conn uid :<|>
  deleteUserPostHandler rs sks conn uid

server :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> Server API
server rs sks conn =
  publicServer rs sks conn :<|>
  usersServer rs sks conn :<|>
  postsServer rs sks conn

-- |Authorization/Registration handlers
loginUserHandler :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> AuthUserData
  -> Handler (Cookied User)
loginUserHandler rs sks conn auth = do
  res <- liftIO $ lookupAuth conn auth
  case res of
    Right user -> addSession' (authUserData user) user
    Left err -> throwError $ err401 {errBody = encode err}
  where
    addSession' = addSession authSettings rs sks
    authUserData (User i login passwd _ _ _ _) =
      AuthUserData (Just i) login passwd

logoutUserHandler :: Handler (Cookied ())
logoutUserHandler = removeSession authSettings ()

-- |User handlers
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
    allUsersHandler' auth = do
      res <- liftIO . withAdminPermissions auth conn $ allUsers conn
      case res of
        Right users -> return users
        Left err ->
          case err of
            WrongAuthCookiesErr -> throw401 err
            AccessDeniedErr -> throw403 err

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
    getUserHandler' _ = do
      res <- liftIO $ getUser conn userid
      case res of
        Just user -> return user
        Nothing -> throw404 UserNotFoundErr

saveUserHandler ::
     Connection
  -> AuthUserData
  -> Handler (Headers '[ Header "Location" String] Int)
saveUserHandler conn auth = do
  res <- liftIO $ saveUser conn auth
  case res of
    Right userid -> return $ addHeader ("/users/" ++ show userid) userid
    Left err -> throw409 err

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
      res <- liftIO . withOwnRestriction auth conn userid $ updateUser conn userid user
      case res of
        Right r -> case r of
          Right () -> return ()
          Left err -> case err of
            UserNotFoundErr -> throw404 err
            ServerErr -> throw500 err
        Left err -> case err of
          WrongAuthCookiesErr -> throw401 err
          AccessDeniedErr -> throw403 err

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
      res <- liftIO . withAdminPermissions auth conn $ deleteUser conn userid
      case res of
        Right 1 -> return ()
        Right 0 -> throw404 UserNotFoundErr
        Right _ -> throw500 ServerErr
        Left err -> case err of
          WrongAuthCookiesErr -> throw401 err
          AccessDeniedErr -> throw403 err

-- |Posts handlers
allUserPostsHandler :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> Int
  -> WithMetadata AuthUserData
  -> Handler (Cookied [BlogPost])
allUserPostsHandler rs sks conn userid =
  cookied authSettings rs sks (Proxy :: Proxy AuthUserData) $ allUserPostsHandler'
  where
    allUserPostsHandler' :: AuthUserData -> Handler [BlogPost]
    allUserPostsHandler' auth = do
      res <- liftIO . withOwnRestriction auth conn userid $ allUserPosts conn userid
      case res of
        Right posts -> return posts
        Left err ->
          case err of
            WrongAuthCookiesErr -> throw401 err
            AccessDeniedErr -> throw403 err

getUserPostHandler :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> Int
  -> Int
  -> WithMetadata AuthUserData
  -> Handler (Cookied BlogPost)
getUserPostHandler rs sks conn userid postid =
  cookied authSettings rs sks (Proxy :: Proxy AuthUserData) $ getUserPostHandler'
  where
    getUserPostHandler' :: AuthUserData -> Handler BlogPost
    getUserPostHandler' auth = do
      res <- liftIO . withOwnRestriction auth conn userid $ getPost conn postid
      case res of
        Right post ->
          case post of
            Just p -> return p
            Nothing -> throw404 PostNotFoundErr
        Left err ->
          case err of
            WrongAuthCookiesErr -> throw401 err
            AccessDeniedErr -> throw403 err

createUserPostHandler :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> Int
  -> BlogPost
  -> WithMetadata AuthUserData
  -> Handler (Cookied Int)
createUserPostHandler rs sks conn uid post =
  cookied authSettings rs sks (Proxy :: Proxy AuthUserData) $ createUserPostHandler'
  where
    createUserPostHandler' :: AuthUserData -> Handler Int
    createUserPostHandler' auth = do
      res <- liftIO . withOwnRestriction auth conn uid $ createPost conn uid post
      case res of
        Right pid -> return pid
        Left err ->
          case err of
            WrongAuthCookiesErr -> throw401 err
            AccessDeniedErr -> throw403 err

updateUserPostHandler :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> Int
  -> Int
  -> BlogPost
  -> WithMetadata AuthUserData
  -> Handler (Cookied ())
updateUserPostHandler rs sks conn uid pid post =
  cookied authSettings rs sks (Proxy :: Proxy AuthUserData) $ updateUserPostHandler'
  where
    updateUserPostHandler' :: AuthUserData -> Handler ()
    updateUserPostHandler' auth = do
      res <- liftIO . withOwnRestriction auth conn uid $ updatePost conn uid pid post
      case res of
        Right updated ->
          case updated of
            Right () -> return ()
            Left err ->
              case err of
                PostNotFoundErr -> throw404 err
                ServerErr -> throw500 err
        Left err ->
          case err of
            WrongAuthCookiesErr -> throw401 err
            AccessDeniedErr -> throw403 err

deleteUserPostHandler :: (ServerKeySet s)
  => RandomSource
  -> s
  -> Connection
  -> Int
  -> Int
  -> WithMetadata AuthUserData
  -> Handler (Cookied ())
deleteUserPostHandler rs sks conn uid pid =
  cookied authSettings rs sks (Proxy :: Proxy AuthUserData) $ deleteUserPostHandler'
  where
    deleteUserPostHandler' :: AuthUserData -> Handler ()
    deleteUserPostHandler' auth = do
      res <- liftIO . withOwnRestriction auth conn uid $ deletePost conn uid pid
      case res of
        Right 1 -> return ()
        Right 0 -> throw404 PostNotFoundErr
        Right _ -> throw500 ServerErr
        Left err -> case err of
          WrongAuthCookiesErr -> throw401 err
          AccessDeniedErr -> throw403 err

-- |Execute action only if authorized as admin
withAdminPermissions
  :: AuthUserData
  -> Connection
  -> IO a
  -> IO (Either BlogError a)
-- Not authorized user
withAdminPermissions (AuthUserData Nothing _ _) _ _ =
  return $ Left WrongAuthCookiesErr
-- Authorized user
withAdminPermissions (AuthUserData (Just i) _ _) conn action = do
  user <- getUser conn i
  case user of
    Just u ->
      if userIsAdmin u
      then do
        res <- action
        return $ Right res
      else return $ Left AccessDeniedErr
    Nothing -> return $ Left WrongAuthCookiesErr

-- |Execute action only if owner or admin
withOwnRestriction
  :: AuthUserData
  -> Connection
  -> Int
  -> IO a
  -> IO (Either BlogError a)
-- Not authorized user
withOwnRestriction (AuthUserData Nothing _ _) _ _ _ =
  return $ Left WrongAuthCookiesErr
-- Authorized user
withOwnRestriction (AuthUserData (Just i) _ _) conn action_id action = do
  user <- getUser conn i
  case user of
    Just u ->
      if userIsAdmin u || action_id == i
      then do
        res <- action
        return $ Right res
      else return $ Left AccessDeniedErr
    Nothing -> return $ Left WrongAuthCookiesErr
