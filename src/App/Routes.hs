{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module App.Routes
  ( AuthAPI
  , UsersAPI
  , API
  ) where

import Servant
import Servant.Server.Experimental.Auth.Cookie
import App.Models (User)

-- |Auth API
type AuthAPI =
  "login" :> ReqBody '[JSON] User :> Post '[JSON] (Cookied User) :<|>
  "logout" :> Get '[JSON] (Cookied ()) :<|>
  "users" :> AuthProtect "cookie-auth" :> Get '[JSON] (Cookied [User])

  

-- |Users API
type UsersAPI =
  Get '[JSON] [User] :<|>
  Capture "id" Int :> Get '[JSON] User :<|>
  ReqBody '[JSON] User :> PostCreated '[JSON] (Headers '[Header "Location" String] Int) :<|>
  Capture "id" Int :> ReqBody '[JSON] User :> Patch '[JSON] () :<|>
  Capture "id" Int :> Delete '[JSON] ()

-- |Common API
type API =
  "users" :> UsersAPI
