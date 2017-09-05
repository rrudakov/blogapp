{-# LANGUAGE OverloadedStrings #-}
module App.Errors
  ( BlogError (..)
  , throw401
  , throw403
  , throw404
  , throw409
  , throw500
  ) where

import Data.Aeson
import Servant
       (Handler, ServantErr(..), err401, err403, err404, err409, err500,
        throwError)

data BlogError = LoginExistErr
               | LoginIncorrectErr
               | WrongPasswordErr
               | DataBaseErr
               | UserNotFoundErr
               | ServerErr
               | NoAuthCookiesErr
               | WrongAuthCookiesErr
               | AccessDeniedErr
               | PostNotFoundErr

instance Show BlogError where
  show LoginExistErr = "LOGIN_ALREADY_EXIST"
  show LoginIncorrectErr = "LOGIN_INCORRECT"
  show WrongPasswordErr = "WRONG_PASSWORD"
  show DataBaseErr = "DATABASE_ERROR"
  show UserNotFoundErr = "USER_NOT_FOUND"
  show ServerErr = "SERVER_ERROR"
  show NoAuthCookiesErr = "NO_AUTH_COOKIES"
  show WrongAuthCookiesErr = "WRONG_AUTH_COOKIES"
  show AccessDeniedErr = "ACCESS_DENIED"
  show PostNotFoundErr = "POST_NOT_FOUND"

instance ToJSON BlogError where
  toJSON err = object ["err" .= show err]

  toEncoding err = pairs ("err" .= show err)

throw401 :: BlogError -> Handler a
throw401 err = throwError $ err401 {errBody = encode err}

throw403 :: BlogError -> Handler a
throw403 err = throwError $ err403 {errBody = encode err}

throw404 :: BlogError -> Handler a
throw404 err = throwError $ err404 {errBody = encode err}

throw409 :: BlogError -> Handler a
throw409 err = throwError $ err409 {errBody = encode err}

throw500 :: BlogError -> Handler a
throw500 err = throwError $ err500 {errBody = encode err}
