{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module App
    ( startApp
    , app
    ) where

import           App.Models
import           App.Routes
import           App.Server
import           Control.Monad.Catch (catch)
import           Crypto.Random (drgNew)
import qualified Data.ByteString.Char8 as BSC8
import           Data.ByteString.Lazy (fromStrict)
import           Data.Default (def)
import           Database.PostgreSQL.Simple (Connection, connectDatabase, connectPassword, connectUser, connectHost, connect, defaultConnectInfo)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import           Servant.Server.Experimental.Auth.Cookie

startApp :: IO ()
startApp = do
  rs <- mkRandomSource drgNew 1000
  -- Connect to database
  conn <- connect defaultConnectInfo
    { connectHost = "192.168.56.170"
    , connectUser = "blogger"
    , connectPassword = "123456"
    , connectDatabase = "blogapp_dev"}
  let sks = mkPersistentServerKey "0123456789abcdef"
  -- Run application
  run 8080 (app conn authSettings rs sks)


authSettings :: AuthCookieSettings
authSettings = def { acsCookieFlags = ["HttpOnly"]}

-- | Custom handler that bluntly reports any occurred errors.
authHandler :: (ServerKeySet s)
  => AuthCookieSettings
  -> s
  -> AuthHandler Request (WithMetadata User)
authHandler acs sks = mkAuthHandler $ \request ->
  (getSession acs sks request) `catch` handleEx >>= maybe
    (throwError err403 {errBody = "No cookies"})
    (return)
  where
    handleEx :: AuthCookieException -> Handler (Maybe (WithMetadata User))
    handleEx ex = throwError err403 {errBody = fromStrict . BSC8.pack $ show ex}


app :: (ServerKeySet s) => Connection -> AuthCookieSettings -> RandomSource -> s -> Application
app conn settings rs sks = serveWithContext
  (Proxy :: Proxy AuthAPI)
  ((authHandler settings sks) :. EmptyContext)
  (server conn settings rs sks)
