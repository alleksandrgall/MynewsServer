{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Internal.Image.Test where

import Control.Exception (Exception, throwIO)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map
import qualified Handlers.DB as DB
import qualified Handlers.Image as I
import Servant.Multipart (FileData (fdPayload))
import Prelude hiding (lookup)

newtype ImageTestIO a = ImageTestIO {unImageTestIO :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

type instance I.ImageEnv ImageTestIO = IORef (Map String BS.ByteString)

data IOMockError = IOMockError deriving (Show)

instance Exception IOMockError

withHandler :: DB.Handler -> (I.Handler ImageTestIO -> IO a) -> IO a
withHandler dbH f = do
  imageEnv <- newIORef mempty :: IO (IORef (Map String BS.ByteString))
  let conf =
        I.Config
          { I.cMaxImageSize = return 10,
            I.cMaxNumberOfImages = return 2
          }
  f
    I.Handler
      { I.hConfig = conf,
        I.hRunImage = unImageTestIO,
        I.hDBHandler = dbH,
        I.hPrepareEnv = return imageEnv,
        I.hPutImage = \env fd -> do
          map <- liftIO $ readIORef env
          let newName = show . length $ map
              newMap = insert newName (toStrict . fdPayload $ fd) map
          liftIO $ writeIORef env newMap
          return newName,
        I.hGetImage = \name -> do
          map <- liftIO $ readIORef imageEnv
          case lookup name map of
            Nothing -> liftIO $ throwIO IOMockError
            Just res -> return res,
        I.hDeleteImage = \name -> do
          map <- liftIO $ readIORef imageEnv
          if member name map
            then liftIO $ writeIORef imageEnv (delete name map)
            else liftIO $ throwIO IOMockError
      }

  return undefined