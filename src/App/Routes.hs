{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module App.Routes
  ( PublicAPI
  , UsersAPI
  , API
  , api
  ) where

import Servant
import App.Models (User, AuthUserData)
import Servant.Server.Experimental.Auth.Cookie

-- |Public API
type PublicAPI =
  "register" :> ReqBody '[JSON] User :> PostCreated '[JSON] (Headers '[Header "Location" String] Int) :<|>
  "login" :> ReqBody '[JSON] AuthUserData :> Post '[JSON] (Cookied User) :<|>
  "logout" :> Get '[JSON] (Cookied ())

-- |Users API
type UsersAPI =
  AuthProtect "cookie-auth" :> Get '[JSON] (Cookied [User]) :<|>
  Capture "id" Int :> AuthProtect "cookie-auth" :> Get '[JSON] (Cookied User) :<|>
  Capture "id" Int :> ReqBody '[JSON] User :> AuthProtect "cookie-auth" :> Patch '[JSON] (Cookied ()) :<|>
  Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] (Cookied ())

-- |Common API
type API =
  PublicAPI :<|>
  "users" :> UsersAPI

api :: Proxy API
api = Proxy
