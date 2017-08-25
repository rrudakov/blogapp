{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module App.Routes
  ( PublicAPI
  , UsersAPI
  , API
  , api
  ) where

import Servant
import App.Models (User)

-- |Public API
type PublicAPI =
  "register" :> ReqBody '[JSON] User :> PostCreated '[JSON] (Headers '[Header "Location" String] Int)

-- |Users API
type UsersAPI =
  Get '[JSON] [User] :<|>
  Capture "id" Int :> Get '[JSON] User :<|>
  Capture "id" Int :> ReqBody '[JSON] User :> Patch '[JSON] () :<|>
  Capture "id" Int :> Delete '[JSON] ()

-- |Common API
type API =
  PublicAPI :<|>
  "users" :> BasicAuth "user-realm" User :> UsersAPI

api :: Proxy API
api = Proxy
