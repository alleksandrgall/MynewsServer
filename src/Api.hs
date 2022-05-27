{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Api.Article (ArticleApi, articleServer)
import Api.Category (CategoryApi, categoryServer)
import Api.Image (ImageApi, imageServer)
import Api.User (UserApi, userServer)
import App (App, serve_)
import Servant (HasServer (ServerT), Proxy (Proxy), type (:<|>) ((:<|>)), type (:>))

type Api =
  "user" :> UserApi
    :<|> "category" :> CategoryApi
    :<|> "article" :> ArticleApi
    :<|> "image" :> ImageApi

serverApi :: ServerT Api App
serverApi = userServer :<|> categoryServer :<|> articleServer :<|> imageServer

api :: Proxy Api
api = Proxy

app :: Int -> IO ()
app = serve_ api serverApi