{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module App.Routes
  ( -- * Types
    PublicAPI
  , UsersAPI
  , PostsAPI
  , API
    -- * Functions
  , api
  ) where

import App.Models (User, UserCredentials, UserWithToken, BlogPost)
import Servant

-- |Public API
type PublicAPI =
  -- /register/ - POST create user
  "register" :> ReqBody '[JSON] UserCredentials :> PostCreated '[JSON] UserWithToken :<|>
  -- /login/ - POST login user
  "login" :> ReqBody '[JSON] UserCredentials :> Post '[JSON] UserWithToken

-- |Users API
type UsersAPI =
  "users" :>
  (
    -- /users/ - GET all users
    Get '[JSON] [User] :<|>
    -- /users/:id/ - GET user by id
    Capture "id" Int :> Get '[JSON] User :<|>
    -- /users/:id/ - PATCH update user by id
    Capture "id" Int :> ReqBody '[JSON] User :> Patch '[JSON] () :<|>
    -- /users/:id/ - DELETE delete user by id
    Capture "id" Int :> Delete '[JSON] ()
  )

-- |Posts API
type PostsAPI =
  "users" :> Capture "id" Int :> "posts" :>
  (
    -- /users/:id/posts/ - GET all user posts
    Get '[JSON] [BlogPost] :<|>
    -- /users/:id/posts/:id/ - GET user post by id
    Capture "id" Int :> Get '[JSON] BlogPost :<|>
    -- /users/:id/posts/ - POST create post
    ReqBody '[JSON] BlogPost :> PostCreated '[JSON] Int :<|>
    -- /users/:id/posts/:id/ - PATCH update post by id
    Capture "id" Int :> ReqBody '[JSON] BlogPost :> Patch '[JSON] () :<|>
    -- /users/:id/posts/:id/ - DELETE delete post by id
    Capture "id" Int :> Delete '[JSON] ()
  )
  

-- |Common API
type API =
  PublicAPI :<|>
  AuthProtect "JWT" :> UsersAPI :<|>
  AuthProtect "JWT" :> PostsAPI

api :: Proxy API
api = Proxy
