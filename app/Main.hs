{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Exception (bracket)
import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Crypto.KDF.BCrypt
import DB.Scheme
import Data.Aeson (Options (fieldLabelModifier), ToJSON, camelTo2, defaultOptions, genericToJSON)
import Data.Aeson.Types (ToJSON (toJSON))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (toGregorian)
import Data.Time.Clock (UTCTime (utctDay), getCurrentTime)
import Database.Esqueleto.Experimental
import qualified Database.Persist as P
import Dev
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Multipart (FileData (fdFileCType, fdInputName, fdPayload), Mem, MultipartData (files), MultipartForm, lookupFile)
import Servant.Server
import System.Directory
import System.IO (hClose)
import System.Posix (mkstemp)

main :: IO ()
main = run 3000 app

createArticle ::
  String -> -- title
  String -> -- content
  Bool -> -- isPublised
  UserId ->
  CategoryId ->
  Article
createArticle t c isP uId cId = Article t uId cId c isP

createUserDev ::
  String -> -- login
  String -> --pswd
  Bool -> --is admin
  IO User
createUserDev log pswd isAdm = do
  pswdH <- (hashPassword 6 $ encodeUtf8 . T.pack $ pswd :: IO BS.ByteString)
  date <- getCurrentTime
  return $
    User
      { userName = log,
        userAvatar = Nothing,
        userPasswordHash = pswdH,
        userCreated = date,
        userIsAdmin = isAdm,
        userIsAuthor = False
      }

type TestApi = "addPictures" :> Capture "articleId" Int64 :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Bool

api :: Proxy TestApi
api = Proxy

maxFiles = 30

imageRoot = "/home/turban/metaLampServer/images"

maxImageSize :: Int64
maxImageSize = 20971520

addPictures :: Int64 -> MultipartData Mem -> Handler Bool
addPictures articleId form = do
  let fls = files form
  when (length fls >= maxFiles) (throwError err413) --too many files
  mapM_ validate fls
  pathes <- liftIO $ saveFiles $ map fdPayload fls
  liftIO . runDBDev $ mapM_ (\(fd, p) -> saveDb (T.unpack . fdFileCType $ fd) p) (zip fls pathes)
  return True
  where
    validate :: FileData Mem -> Handler ()
    validate fd = do
      when (T.takeWhile (/= '/') (fdFileCType fd) /= "image") (throwError err415) --not an image
      when (LBS.length (fdPayload fd) > maxImageSize) (throwError err413) --file too large
    saveDb :: String -> FilePath -> SqlPersistM ()
    saveDb mime fp = void $ insert $ Image mime fp (ArticleKey . SqlBackendKey $ articleId)

    saveFiles :: [LBS.ByteString] -> IO [FilePath]
    saveFiles lbss = do
      (year, month', day') <- toGregorian . utctDay <$> getCurrentTime
      let trg = imageRoot ++ "/" ++ show year ++ "/" ++ show month' ++ "/" ++ show day'
      createDirectoryIfMissing True trg
      mapM
        ( \lbs ->
            bracket
              (mkstemp (trg ++ "/" ++ "image"))
              (hClose . snd)
              (\(fp, h) -> LBS.hPut h lbs >> return fp)
        )
        lbss

app :: Application
app = serve api addPictures