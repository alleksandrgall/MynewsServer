{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module App.MiddlewareT where

import App.App
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Network.Wai as W
import Servant
import UnliftIO

type ApplicationT m = W.Request -> (W.Response -> AppT m W.ResponseReceived) -> AppT m W.ResponseReceived

type MiddlewareT m = ApplicationT m -> ApplicationT m

-- toApplicationT ::
--   ( MonadIO m,
--     HasServer api context,
--     HasContextEntry
--       (context .++ DefaultErrorFormatters)
--       ErrorFormatters
--   ) =>
--   Proxy api ->
--   ServerT api (AppT m) ->
--   Context context ->
--   AppConfig ->
--   ApplicationT m
-- toApplicationT prApi server ctxt appConf req respReceived = do
--   withRunInIO $ \runInIO -> serveWithContext prApi ctxt (toServer prApi appConf server) req (runInIO . respReceived)
