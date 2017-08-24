module App.Errors where

loginExistErr :: String
loginExistErr = "LOGIN_ALREADY_EXIST"

loginIncorrectErr :: String
loginIncorrectErr = "LOGIN_INCORRECT"

wrongPasswordErr :: String
wrongPasswordErr = "WRONG_PASSWORD"

dataBaseErr :: String
dataBaseErr = "DATABASE_ERROR"

userNotFoundErr :: String
userNotFoundErr = "USER_NOT_FOUND"

serverErr :: String
serverErr = "SERVER_ERROR"
