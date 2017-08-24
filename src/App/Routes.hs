{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module App.Routes
  ( UsersAPI
  , API
  ) where

import Servant
import App.Models (User)
  

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
