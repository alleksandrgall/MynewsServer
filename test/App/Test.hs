module App.Test (Handler, withHandler) where

import Handlers.App
import qualified Handlers.DB as DB
import qualified Handlers.Katip as L
import System.Directory (getCurrentDirectory)
import System.IO.Temp (withTempDirectory)

withHandler :: DB.Handler -> L.Handler -> (Handler -> IO a) -> IO a
withHandler dbH lH f = do
  currentDir <- getCurrentDirectory
  withTempDirectory currentDir "imageStorage" $ \imageDir ->
    f $
      Handler
        { hConfig =
            Config
              { cImageRoot = return imageDir,
                cMaxImageSize = return 20000000,
                cPaginationLimit = return 5,
                cMaxImagesUpload = return 5
              },
          hDBHandler = dbH,
          hKatipHandler = lH
        }