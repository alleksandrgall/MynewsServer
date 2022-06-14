{-# LANGUAGE TypeFamilies #-}

module Image.File (parseConfig, withHandler) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Time (UTCTime (utctDay), getCurrentTime, toGregorian)
import Handlers.App (askImageRoot)
import qualified Handlers.DB as DB
import Handlers.Image (Config (..), Handler (..), ImageEnv)
import Servant.Multipart (FileData (fdFileName, fdPayload), Mem)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (takeExtension, (</>))
import System.IO (hClose)
import System.Posix (mkstemps)

newtype FatalImageConfigError = FatalImageConfigError String deriving (Show)

instance Exception FatalImageConfigError

confImageRoot :: C.Config -> IO String
confImageRoot c = do
  maybeVal <- C.lookup c "imageRoot"
  maybe (throwIO $ FatalImageConfigError "Location for image storage is required") return maybeVal

confMaxImageSize :: C.Config -> IO Int64
confMaxImageSize c = do
  maybeVal <- C.lookup c "maxImageSize"
  maybe (throwIO $ FatalImageConfigError "Maximum size of image must be provided (in bytes)") return maybeVal

confMaxImagesUpload :: C.Config -> IO Int
confMaxImagesUpload c = do
  maybeVal <- C.lookup c "maxImagesUpload"
  maybe (throwIO $ FatalImageConfigError "Maximum number of simultaneously uploading images must be provided") return maybeVal

parseConfig :: C.Config -> IO (Config IO)
parseConfig conf =
  return $
    Config
      { cMaxImageSize = confMaxImageSize conf,
        cMaxNumberOfImages = confMaxImagesUpload conf
      }

type instance ImageEnv IO = FilePath

prepareFileEnv :: C.Config -> IO FilePath
prepareFileEnv conf = do
  (year, month', day') <- liftIO $ toGregorian . utctDay <$> getCurrentTime
  imageRoot <- confImageRoot conf
  let trg = imageRoot </> show year </> show month' </> show day'
  liftIO $ createDirectoryIfMissing True trg
  return trg

putInFile :: FilePath -> FileData Mem -> IO FilePath
putInFile imageRoot fd = do
  (imageFp, h) <- liftIO $ mkstemps imageRoot (takeExtension (T.unpack $ fdFileName fd))
  LBS.hPut h (fdPayload fd)
  hClose h
  return imageFp

getImageFile :: FilePath -> IO BS.ByteString
getImageFile = BS.readFile

deleteImageFile :: FilePath -> IO ()
deleteImageFile = removeFile

withHandler :: C.Config -> DB.Handler -> (Handler IO -> IO ()) -> IO ()
withHandler conf dbH f = do
  imageConf <- parseConfig conf
  f $
    Handler
      { hConfig = imageConf,
        hDBHandler = dbH,
        hPrepareEnv = prepareFileEnv conf,
        hPutImage = putInFile,
        hGetImage = getImageFile,
        hDeleteImage = deleteImageFile
      }