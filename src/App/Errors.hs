{-# LANGUAGE OverloadedStrings #-}
module App.Errors where

import Data.Aeson

data BlogError = LoginExistErr
               | LoginIncorrectErr
               | WrongPasswordErr
               | DataBaseErr
               | UserNotFoundErr
               | ServerErr

instance Show BlogError where
  show LoginExistErr = "LOGIN_ALREADY_EXIST"
  show LoginIncorrectErr = "LOGIN_INCORRECT"
  show WrongPasswordErr = "WRONG_PASSWORD"
  show DataBaseErr = "DATABASE_ERROR"
  show UserNotFoundErr = "USER_NOT_FOUND"
  show ServerErr = "SERVER_ERROR"

instance ToJSON BlogError where
  toJSON err = object ["err" .= show err]

  toEncoding err = pairs ("err" .= show err)
