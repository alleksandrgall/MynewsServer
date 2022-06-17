{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Api.Article (ArticleApi, articleServer)
import Api.Category (CategoryApi, categoryServer)
import Api.Image (ImageApi, imageServer)
import Api.User (UserApi, userServer)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Handlers.App (App, Handler, serve_)
import qualified Network.Wai as W
import Servant (HasServer (ServerT), Proxy (Proxy), type (:<|>) ((:<|>)), type (:>))

type Api =
  "user" :> UserApi
    :<|> "category" :> CategoryApi
    :<|> "article" :> ArticleApi
    :<|> "image" :> ImageApi

serverApi :: (MonadMask imageM, MonadIO imageM) => ServerT Api (App imageM)
serverApi = userServer :<|> categoryServer :<|> articleServer :<|> imageServer

api :: Proxy Api
api = Proxy

app :: (MonadMask imageM, MonadIO imageM) => Handler imageM -> W.Application
app h = serve_ h api serverApi
