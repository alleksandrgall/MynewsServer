{-# LANGUAGE OverloadedStrings #-}

module Katip.Test (Handler, withHandler) where

import Handlers.Katip (Config (Config), Handler (..))
import Katip (initLogEnv)

withHandler :: (Handler -> IO a) -> IO a
withHandler f = do
  logEnv <- initLogEnv "test" "main"
  f $ Handler $ Config logEnv mempty mempty