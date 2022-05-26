{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Api.Article
import Api.Category
import Api.User
import qualified App.App as E
import App.Middleware (mkApplication')
import qualified App.Middleware as E
import DB.Scheme
import Database.Persist.Sql (runMigration)
import Dev
import Katip (Severity (InfoS))
import KatipMiddleware
import Network.Wai.Handler.Warp
import Servant

type Api =
  "user" :> UserApi
    :<|> "category" :> CategoryApi
    :<|> "article" :> ArticleApi

app = userServer :<|> categoryServer :<|> articleServer

main :: IO ()
main = E.withAppConfig (\config -> run 3000 . E.runApplicationM (E.logConfig config) . E.katipMiddlewareInternal InfoS . E.mkApplication' (Proxy @Api) app $ config)

-- runDBDev $ runMigration migrateAll
-- withAppConfig (\config -> )run 3000 . katipMiddleware (logConfig config) InfoS . serveApp (Proxy :: Proxy Api) app $ config