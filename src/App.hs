{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module App (module X, serve_) where

import App.App (App, AppConfig (logConfig), serveApp, withAppConfig)
import App.App as X (App, AppT, askImageRoot, askMaxImageSize, askMaxImagesUpload, askPaginationLimit, runDB)
import App.Auth (AuthContext)
import App.Auth as X (Auth (Auth))
import App.Middleware (katipMiddleware, mkApplicationK, runApplicationK)
import Katip (Severity (InfoS))
import Network.Wai.Handler.Warp (run)
import Servant (Application, HasServer (ServerT), Proxy)

serveWithKatip :: HasServer api AuthContext => Proxy api -> ServerT api App -> AppConfig -> Application
serveWithKatip api appServer appConfig =
  let withMiddleWare = katipMiddleware InfoS . mkApplicationK $ \localLogConfig -> serveApp api appServer (appConfig {logConfig = localLogConfig})
   in runApplicationK (logConfig appConfig) withMiddleWare

serve_ :: HasServer api AuthContext => Proxy api -> ServerT api App -> Int -> IO ()
serve_ api server port = withAppConfig $ run port . serveWithKatip api server