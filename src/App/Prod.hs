module App.Prod (Handler, parseConfig, withHandler) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Ratio (numerator)
import GHC.Natural (Natural)
import GHC.Real (denominator)
import Handlers.App (App, Config (..), Handler (..))
import qualified Handlers.DB as DB
import qualified Handlers.Image as I
import qualified Handlers.Katip as L

newtype FatalConfigError = FatalConfigError String deriving (Show)

instance Exception FatalConfigError

instance C.Configured Natural where
  convert (C.Number r) = case (numerator r, denominator r) of
    (x, 1) -> if x > 0 then Just . fromIntegral $ x else Nothing
    (_, _) -> Nothing
  convert _ = Nothing

confPaginationLimit :: C.Config -> App imageM Natural
confPaginationLimit c = do
  maybeVal <- liftIO . C.lookup c $ "paginationLimit"
  maybe (liftIO . throwIO $ FatalConfigError "Default pagination limit must be provided") return maybeVal

parseConfig :: C.Config -> IO (Config imageM)
parseConfig conf = do
  return $
    Config
      { cPaginationLimit = confPaginationLimit conf
      }

withHandler :: L.Handler -> DB.Handler -> I.Handler imageM -> Config imageM -> (Handler imageM -> IO a) -> IO a
withHandler lHand dbHand imHand config f = do
  f $
    Handler
      { hKatipHandler = lHand,
        hDBHandler = dbHand,
        hImageHandler = imHand,
        hConfig = config
      }
