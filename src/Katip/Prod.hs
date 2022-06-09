{-# LANGUAGE RankNTypes #-}

module Katip.Prod (Handler, parseConfig, withHandler) where

import Data.Char (toLower)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import Data.Text.Internal.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as T
import Handlers.Katip
import qualified Katip as K
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

newtype MySeverity = MySeverity K.Severity

instance C.Configured MySeverity where
  convert (C.String s) =
    case T.map toLower s of
      "debug" -> Just $ MySeverity K.DebugS
      "info" -> Just $ MySeverity K.InfoS
      "warn" -> Just $ MySeverity K.WarningS
      "error" -> Just $ MySeverity K.ErrorS
      "notice" -> Just $ MySeverity K.NoticeS
      _ -> Nothing
  convert _ = Nothing

newtype MyVerbosity = MyVerbosity K.Verbosity

instance C.Configured MyVerbosity where
  convert (C.String s) =
    case T.map toLower s of
      "v0" -> Just $ MyVerbosity K.V0
      "v1" -> Just $ MyVerbosity K.V1
      "v2" -> Just $ MyVerbosity K.V2
      "v3" -> Just $ MyVerbosity K.V3
      _ -> Nothing
  convert _ = Nothing

data LogOut = Stdout | Stderr | Silent

instance C.Configured LogOut where
  convert (C.String s) =
    case T.map toLower s of
      "stdout" -> Just Stdout
      "stderr" -> Just Stderr
      "silent" -> Just Silent
      _ -> Nothing
  convert _ = Nothing

mkScribeFromConfig :: C.Config -> IO K.Scribe
mkScribeFromConfig cnf = return $ K.Scribe write finale permit
  where
    write :: forall a. K.LogItem a => K.Item a -> IO ()
    write i = do
      out <- C.lookupDefault Stdout cnf "logOut"
      (MyVerbosity verb) <- C.lookupDefault (MyVerbosity K.V2) cnf "logVerb"
      case out of
        Silent -> return ()
        Stdout -> do
          hSetBuffering stdout LineBuffering
          T.hPutStrLn stdout $ toLazyText $ K.bracketFormat True verb i
        Stderr -> do
          hSetBuffering stderr LineBuffering
          T.hPutStrLn stderr $ toLazyText $ K.bracketFormat True verb i
    finale :: IO ()
    finale = return ()
    permit :: K.PermitFunc
    permit i = do
      (MySeverity sev) <- C.lookupDefault (MySeverity K.InfoS) cnf "logLevel"
      K.permitItem sev i

parseConfig :: C.Config -> IO Config
parseConfig conf = do
  scrb <- mkScribeFromConfig conf
  appScribeLogEnv <- K.registerScribe "live scribe" scrb K.defaultScribeSettings =<< K.initLogEnv "metaLampServer" "main"
  return $ Config appScribeLogEnv mempty mempty

withHandler :: Config -> (Handler -> IO a) -> IO a
withHandler conf f = f . Handler $ conf