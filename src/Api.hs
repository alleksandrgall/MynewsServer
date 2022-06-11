{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Api.Article (ArticleApi, articleServer)
import Api.Category (CategoryApi, categoryServer)
import Api.Image (ImageApi, imageServer)
import Api.User (UserApi, userServer)
import Handlers.App (App, Handler, serve_)
import qualified Network.Wai as W
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

app :: Handler -> W.Application
app h = serve_ h api serverApi