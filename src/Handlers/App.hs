{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.App (serve_, module X) where

import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Data (Proxy (Proxy))
import Handlers.App.App as X
  ( App (..),
    Config (..),
    Handler (..),
    askPaginationLimit,
    runDB,
  )
import Handlers.App.Auth (AuthContext, authContext)
import Handlers.App.Auth as X (Auth (..))
import Handlers.Katip.Middleware (katipMiddleware, mkApplicationK, runApplicationK)
import Katip (Severity (InfoS))
import qualified Network.Wai as W
import qualified Servant as S

convertApp :: Handler imageM -> App imageM a -> S.Handler a
convertApp h (App m) =
  liftIO (runExceptT . runReaderT m $ h) >>= \case
    Right v -> pure v
    Left err -> throwError err

toServer :: S.HasServer api AuthContext => Handler imageM -> Proxy api -> S.ServerT api (App imageM) -> S.Server api
toServer h api = S.hoistServerWithContext api (Proxy @AuthContext) (convertApp h)

serveApp :: S.HasServer api AuthContext => Handler imageM -> Proxy api -> S.ServerT api (App imageM) -> W.Application
serveApp h api appServer = S.serveWithContext api (authContext h) (toServer h api appServer)

serveWithKatip :: S.HasServer api AuthContext => Handler imageM -> Proxy api -> S.ServerT api (App imageM) -> W.Application
serveWithKatip h api appServer =
  let withMiddleWare = katipMiddleware InfoS . mkApplicationK $ \localKatipHandler ->
        serveApp (h {hKatipHandler = localKatipHandler}) api appServer
   in runApplicationK (hKatipHandler h) withMiddleWare

serve_ :: S.HasServer api AuthContext => Handler imageM -> Proxy api -> S.ServerT api (App imageM) -> W.Application
serve_ = serveWithKatip