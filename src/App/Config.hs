{-# LANGUAGE OverloadedStrings #-}
module App.Config
  ( BlogConfig (..)
  , ServerConfig (..)
  , blogConnectInfo
  ) where

import Data.Aeson
import Database.PostgreSQL.Simple (ConnectInfo (..))

data DBConfig = DBConfig
  { dbHost :: String
  , dbUser :: String
  , dbPassword :: String
  , dbName :: String
  } deriving (Show)

instance FromJSON DBConfig where
  parseJSON = withObject "DBConfig" $ \ v -> DBConfig
    <$> v .: "host"
    <*> v .: "username"
    <*> v .: "password"
    <*> v .: "database_name"

data ServerConfig = ServerConfig
  { serverPort :: Int
  } deriving (Show)

instance FromJSON ServerConfig where
  parseJSON = withObject "DBConfig" $ \ v -> ServerConfig
    <$> v .: "port"

data BlogConfig = BlogConfig
  { blogDB :: DBConfig
  , blogServer :: ServerConfig
  } deriving (Show)

instance FromJSON BlogConfig where
  parseJSON = withObject "BlogConfig" $ \ v -> BlogConfig
    <$> v .: "database"
    <*> v .: "server"

blogConnectInfo :: DBConfig -> ConnectInfo
blogConnectInfo conf = ConnectInfo
  { connectHost = host
  , connectPort = 5432
  , connectUser = user
  , connectPassword = pass
  , connectDatabase = db
  }
  where
    host = dbHost conf
    user = dbUser conf
    pass = dbPassword conf
    db = dbName conf
