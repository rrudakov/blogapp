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

import App.Models (User, AuthUserData, BlogPost)
import Servant
import Servant.Server.Experimental.Auth.Cookie

-- |Public API
type PublicAPI =
  -- /register/ - POST create user
  "register" :> ReqBody '[JSON] AuthUserData :> PostCreated '[JSON] (Headers '[Header "Location" String] Int) :<|>
  -- /login/ - POST login user
  "login" :> ReqBody '[JSON] AuthUserData :> Post '[JSON] (Cookied User) :<|>
  -- /logout/ - GET logout user
  "logout" :> Get '[JSON] (Cookied ())

-- |Users API
type UsersAPI =
  "users" :>
  (
    -- /users/ - GET all users
    AuthProtect "cookie-auth" :> Get '[JSON] (Cookied [User]) :<|>
    -- /users/:id/ - GET user by id
    Capture "id" Int :> AuthProtect "cookie-auth" :> Get '[JSON] (Cookied User) :<|>
    -- /users/:id/ - PATCH update user by id
    Capture "id" Int :> ReqBody '[JSON] User :> AuthProtect "cookie-auth" :> Patch '[JSON] (Cookied ()) :<|>
    -- /users/:id/ - DELETE delete user by id
    Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] (Cookied ())
  )

-- |Posts API
type PostsAPI =
  "users" :> Capture "id" Int :> "posts" :>
  (
    -- /users/:id/posts/ - GET all user posts
    AuthProtect "cookie-auth" :> Get '[JSON] (Cookied [BlogPost]) :<|>
    -- /users/:id/posts/:id/ - GET user post by id
    Capture "id" Int :> AuthProtect "cookie-auth" :> Get '[JSON] (Cookied BlogPost) :<|>
    -- /users/:id/posts/ - POST create post
    ReqBody '[JSON] BlogPost :> AuthProtect "cookie-auth" :> PostCreated '[JSON] (Cookied Int) :<|>
    -- /users/:id/posts/:id/ - PATCH update post by id
    Capture "id" Int :> ReqBody '[JSON] BlogPost :> AuthProtect "cookie-auth" :> Patch '[JSON] (Cookied ()) :<|>
    -- /users/:id/posts/:id/ - DELETE delete post by id
    Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] (Cookied ())
  )
  

-- |Common API
type API =
  PublicAPI :<|>
  UsersAPI :<|>
  PostsAPI

api :: Proxy API
api = Proxy
