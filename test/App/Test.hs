module App.Test (Handler, withHandler) where

import Handlers.App
import qualified Handlers.DB as DB
import qualified Handlers.Image as I
import qualified Handlers.Katip as L
import System.Directory (getCurrentDirectory)
import System.IO.Temp (withTempDirectory)

withHandler :: DB.Handler -> L.Handler -> I.Handler imageM -> (Handler imageM -> IO a) -> IO a
withHandler dbH lH imH f =
  f $
    Handler
      { hConfig = Config {cPaginationLimit = return 3},
        hDBHandler = dbH,
        hKatipHandler = lH,
        hImageHandler = imH
      }