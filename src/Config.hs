module Config where

import Control.Concurrent (killThread)
import Control.Exception (bracket, throwIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Network.Wai.Handler.Warp (Port)

withConfig :: FilePath -> (C.Config -> IO a) -> IO a
withConfig confPath f =
  bracket
    (C.autoReload (C.autoConfig {C.onError = throwIO}) [C.Required confPath])
    (\(_, threadId) -> killThread threadId)
    (\(c, _) -> f c)

askPort :: C.Config -> IO Port
askPort c = C.lookupDefault 3000 c "port"