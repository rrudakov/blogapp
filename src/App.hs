{-# LANGUAGE OverloadedStrings #-}
module App
    ( startApp
    , app
    ) where

import App.Config (BlogConfig (..), ServerConfig (..), blogConnectInfo)
import App.Routes (api)
import App.Server (server, authContext)
import Crypto.Random (drgNew)
import Data.Yaml.Config
import Database.PostgreSQL.Simple (Connection, connectDatabase, connectPassword, connectUser, connectHost, connect, defaultConnectInfo)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Servant.Server.Experimental.Auth.Cookie

startApp :: IO ()
startApp = do
  -- Read config file
  config <- loadYamlSettings ["blogconfig.yml"] [] ignoreEnv
  let dbConfig = blogDB config
  let serverConfig = blogServer config
  -- Connect to database
  conn <- connect $ blogConnectInfo dbConfig
  -- Some encryption staff
  let sks = mkPersistentServerKey "0123456789abcdef"
  rs <- mkRandomSource drgNew 1000
  -- Run application
  run (serverPort serverConfig) (app rs sks conn)


myCors :: Middleware
myCors = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
      { corsRequestHeaders = ["Content-Type"]
      , corsMethods = "PUT" : "PATCH" : "DELETE": simpleMethods}

app :: (ServerKeySet s) => RandomSource -> s -> Connection -> Application
app rs s conn = myCors $ serveWithContext api (authContext s) (server rs s conn)
