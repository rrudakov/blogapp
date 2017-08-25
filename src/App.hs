module App
    ( startApp
    , app
    ) where

import App.Config (BlogConfig (..), ServerConfig (..), blogConnectInfo)
import App.Routes (api)
import App.Server (server, basicAuthServerContext)
import Data.Yaml.Config
import Database.PostgreSQL.Simple (Connection, connectDatabase, connectPassword, connectUser, connectHost, connect, defaultConnectInfo)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

startApp :: IO ()
startApp = do
  -- Read config file
  config <- loadYamlSettings ["blogconfig.yml"] [] ignoreEnv
  let dbConfig = blogDB config
  let serverConfig = blogServer config
  -- Connect to database
  conn <- connect $ blogConnectInfo dbConfig
  -- Run application
  run (serverPort serverConfig) (app conn)


app :: Connection -> Application
app conn = serveWithContext api (basicAuthServerContext conn) (server conn)
