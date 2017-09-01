{-# LANGUAGE OverloadedStrings #-}
module Main where

import App.Config (BlogConfig (..), ServerConfig (..), blogConnectInfo)
import Control.Monad
import Crypto.BCrypt
import Data.ByteString.Char8 (pack)
import Data.Yaml.Config
import Database.PostgreSQL.Simple

main :: IO ()
main = do
  -- Read config file
  config <- loadYamlSettings ["blogconfig.yml"] [] ignoreEnv
  let dbConfig = blogDB config
  -- Connect to database
  conn <- connect $ blogConnectInfo dbConfig
  putStrLn "Enter password for admin user:"
  password <- getLine
  putStrLn "Confirm password:"
  password' <- getLine
  unless (password == password') $ main
  h <- hashPasswordUsingPolicy fastBcryptHashingPolicy (pack password)
  case h of
    Just hash -> do
      res <- execute conn "INSERT INTO users (username, password, created_at, is_active, is_admin) VALUES (?, ?, now(), ?, ?)" ("admin" :: String, h, True, True)
      case res of
        1 -> putStrLn "Successfull"
        _ -> putStrLn "Unable to inserd password in database"
    Nothing -> putStrLn "Unable to encrypt password"
