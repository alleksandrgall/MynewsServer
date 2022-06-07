{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.App (serve_, module X) where

import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Data (Proxy (Proxy))
import Handlers.App.App as X
  ( App (..),
    Config (..),
    ConfigDefault (..),
    Handler (..),
    askImageRoot,
    askMaxImageSize,
    askMaxImagesUpload,
    askPaginationLimit,
    runDB,
  )
import Handlers.App.Auth (AuthContext, authContext)
import Handlers.App.Auth as X (Auth (..))
import Handlers.Katip.Middleware (katipMiddleware, mkApplicationK, runApplicationK)
import Katip (Severity (InfoS))
import qualified Network.Wai as W
import Network.Wai.Handler.Warp (run)
import qualified Servant as S

convertApp :: Handler -> App a -> S.Handler a
convertApp h (App m) =
  liftIO (runExceptT . runReaderT m $ h) >>= \case
    Right v -> pure v
    Left err -> throwError err

toServer :: S.HasServer api AuthContext => Handler -> Proxy api -> S.ServerT api App -> S.Server api
toServer h api = S.hoistServerWithContext api (Proxy @AuthContext) (convertApp h)

serveApp :: S.HasServer api AuthContext => Handler -> Proxy api -> S.ServerT api App -> W.Application
serveApp h api appServer = S.serveWithContext api (authContext h) (toServer h api appServer)

serveWithKatip :: S.HasServer api AuthContext => Handler -> Proxy api -> S.ServerT api App -> S.Application
serveWithKatip h api appServer =
  let withMiddleWare = katipMiddleware InfoS . mkApplicationK $ \localKatipHandler ->
        serveApp (h {hKatipHandler = localKatipHandler}) api appServer
   in runApplicationK (hKatipHandler h) withMiddleWare

serve_ :: S.HasServer api AuthContext => Handler -> Proxy api -> S.ServerT api App -> Int -> IO ()
serve_ h api server port = run port . serveWithKatip h api $ server