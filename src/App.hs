module App
    ( startApp
    , app
    ) where

import           App.Models
import           App.Routes
import           App.Server
import           Database.PostgreSQL.Simple (Connection, connectDatabase, connectPassword, connectUser, connectHost, connect, defaultConnectInfo)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

startApp :: IO ()
startApp = do
  -- Connect to database
  conn <- connect defaultConnectInfo
    { connectHost = "192.168.56.170"
    , connectUser = "blogger"
    , connectPassword = "123456"
    , connectDatabase = "blogapp_dev"}
  -- Run application
  run 8080 $ app conn


app :: Connection -> Application
app conn = serve api $ server conn
