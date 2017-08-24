{-# LANGUAGE OverloadedStrings #-}
module Main where

import Crypto.BCrypt
import Control.Monad
import Data.ByteString.Char8 (pack)
import           Database.PostgreSQL.Simple

main :: IO ()
main = do
  -- Connect to database
  conn <- connect defaultConnectInfo
    { connectHost = "192.168.56.170"
    , connectUser = "blogger"
    , connectPassword = "123456"
    , connectDatabase = "blogapp_dev"}
  putStrLn "Enter password for admin user:"
  password <- getLine
  putStrLn "Confirm password:"
  password' <- getLine
  unless (password == password') $ main
  h <- hashPasswordUsingPolicy fastBcryptHashingPolicy (pack password)
  case h of
    Just hash -> do
      res <- execute conn "INSERT INTO users (id, username, password) VALUES (?, ?, ?)" (1 :: Int, "admin" :: String, h)
      case res of
        1 -> putStrLn "Successfull"
        _ -> putStrLn "Unable to inserd password in database"
    Nothing -> putStrLn "Unable to encrypt password"
