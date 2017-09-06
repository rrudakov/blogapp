{-# LANGUAGE OverloadedStrings #-}
module App
    ( startApp
    , app
    ) where

import App.Config
       (BlogConfig(..), ServerConfig(..), blogConnectInfo)
import App.Routes (api)
import App.Server (authContext, server)
import Data.Yaml.Config
import Data.Text (pack)
import Database.PostgreSQL.Simple
       (Connection, connect, connectDatabase, connectHost,
        connectPassword, connectUser, defaultConnectInfo)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Web.JWT (Secret, secret)

startApp :: IO ()
startApp = do
  -- Read config file
  config <- loadYamlSettings ["blogconfig.yml"] [] ignoreEnv
  let dbConfig = blogDB config
  let serverConfig = blogServer config
  -- Connect to database
  conn <- connect $ blogConnectInfo dbConfig
  -- Some encryption staff
  let key = secret . pack $ blogSecret config
  -- Run application
  run (serverPort serverConfig) (app key conn)


myCors :: Middleware
myCors = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
      { corsRequestHeaders = "X-Requested-With": "Content-Type" : simpleHeaders
      , corsMethods = "PUT" : "PATCH" : "DELETE": "OPTIONS" : simpleMethods
      }

app :: Secret -> Connection -> Application
app key conn =
  myCors $ serveWithContext api (authContext key conn) (server key conn)
