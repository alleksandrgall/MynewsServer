{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logger where

import Control.Monad.Logger
import Data.Configurator.Types
import Katip

logLevelToSeverity :: LogLevel -> Severity
logLevelToSeverity LevelDebug = DebugS
logLevelToSeverity LevelInfo = InfoS
logLevelToSeverity LevelWarn = WarningS
logLevelToSeverity LevelError = ErrorS
logLevelToSeverity (LevelOther _) = NoticeS
