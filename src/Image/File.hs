module Image.File where

import Control.Monad.Catch (MonadThrow (throwM), handle)
import Control.Monad.Cont (foldM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Time (UTCTime (utctDay), getCurrentTime, toGregorian)
import Handlers.Image
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Posix (mkstemps)

writeToFiles :: FilePath -> NonEmpty (FileExt, LBS.ByteString) -> IO (NonEmpty FilePath)
writeToFiles imageRoot fInfs = do
  (year, month', day') <- liftIO $ toGregorian . utctDay <$> getCurrentTime
  let trgDir = imageRoot </> show year </> show month' </> show day'
  liftIO $ createDirectoryIfMissing True trgDir
  case fInfs of
    (FileExt ext, imageBytes) :| [] -> do
      (imageFp, h) <- mkstemps trgDir ext
      handle (handlePut imageFp) (LBS.hPut h imageBytes)
      return undefined
  where
    -- foldM (\acc (FileExt ext, imageBytes) -> do
    --     (imageFp, h) <- mkstemps trgDir ext
    --     onException (LBS.hPut h imageBytes))
    -- return undefined
    handlePut imageFp e = throwM $ PutException imageFp e