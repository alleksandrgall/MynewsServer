module App.Prod (Handler, parseConfig, withHandler) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Int (Int64)
import Data.Ratio (numerator)
import GHC.Natural (Natural)
import GHC.Real (denominator)
import Handlers.App (App, Config (..), Handler (..))
import qualified Handlers.DB as DB
import qualified Handlers.Katip as L

newtype FatalConfigError = FatalConfigError String deriving (Show)

instance Exception FatalConfigError

confImageRoot :: C.Config -> App String
confImageRoot c = do
  maybeVal <- liftIO . C.lookup c $ "imageRoot"
  maybe (liftIO . throwIO $ FatalConfigError "Location for image storage is required") return maybeVal

confMaxImageSize :: C.Config -> App Int64
confMaxImageSize c = do
  maybeVal <- liftIO . C.lookup c $ "maxImageSize"
  maybe (liftIO . throwIO $ FatalConfigError "Maximum size of image must be provided (in bytes)") return maybeVal

instance C.Configured Natural where
  convert (C.Number r) = case (numerator r, denominator r) of
    (x, 1) -> if x > 0 then Just . fromIntegral $ x else Nothing
    (_, _) -> Nothing
  convert _ = Nothing

confPaginationLimit :: C.Config -> App Natural
confPaginationLimit c = do
  maybeVal <- liftIO . C.lookup c $ "paginationLimit"
  maybe (liftIO . throwIO $ FatalConfigError "Default pagination limit must be provided") return maybeVal

confMaxImagesUpload :: C.Config -> App Int
confMaxImagesUpload c = do
  maybeVal <- liftIO . C.lookup c $ "maxImagesUpload"
  maybe (liftIO . throwIO $ FatalConfigError "Maximum number of simultaneously uploading images must be provided") return maybeVal

parseConfig :: C.Config -> IO Config
parseConfig conf = do
  return $
    Config
      { cImageRoot = confImageRoot conf,
        cPaginationLimit = confPaginationLimit conf,
        cMaxImagesUpload = confMaxImagesUpload conf,
        cMaxImageSize = confMaxImageSize conf
      }

withHandler :: L.Handler -> DB.Handler -> Config -> (Handler -> IO a) -> IO a
withHandler lHand dbHand config f = do
  f $
    Handler
      { hKatipHandler = lHand,
        hDBHandler = dbHand,
        hConfig = config
      }
