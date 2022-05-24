{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Api.Article
import Api.Category
import Api.User
import App
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
main = withAppConfig (\config -> run 3000 . katipMiddleware (logConfig config) InfoS . serveApp (Proxy :: Proxy Api) app $ config)

-- runDBDev $ runMigration migrateAll
-- withAppConfig (\config -> )run 3000 . katipMiddleware (logConfig config) InfoS . serveApp (Proxy :: Proxy Api) app $ config