{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Exception (bracket)
import Control.Monad (void, when)
import Control.Monad.Except (MonadTrans (lift), throwError)
import Control.Monad.IO.Class
import Crypto.KDF.BCrypt
import DB.Scheme
import DB.Types.Internal
import Data.Aeson (Options (fieldLabelModifier), ToJSON, camelTo2, decode, decode', defaultOptions, genericToJSON)
import Data.Aeson.Types (ToJSON (toJSON))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Data.Semigroup (Option (Option))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (toGregorian)
import Data.Time.Clock (UTCTime (utctDay), getCurrentTime)
import Database.Esqueleto.Experimental
import qualified Database.Persist as P
import Dev
import GHC.Base (Alternative ((<|>)), Symbol)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Multipart (FileData (fdFileCType, fdFileName, fdInputName, fdPayload), Mem, MultipartData (files), MultipartForm, lookupFile, lookupInput)
import Servant.Server
import System.Directory
import System.IO (hClose)
import System.Posix (UserEntry (userPassword), mkstemps)
import Text.Read (readMaybe)

main :: IO ()
main = do
  u <- createUser "admin" "admin" Nothing True False
  void $ saveUserDBDev u

-- runDBDev $ do
-- u <- liftIO $ createUserDev "UserWithNoArts" "87654" False
-- return undefined

createArticle ::
  String -> -- title
  String -> -- content
  Bool -> -- isPublised
  UserId ->
  CategoryId ->
  Article
createArticle t c isP uId cId = Article t uId cId c isP

saveUserDBDev :: User -> IO UserId
saveUserDBDev u = runDBDev $ insert u

type ApiAddUser = BasicAuth "admin" User :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Ok

type TestApi =
  "add_pictures" :> QueryParam "article_id" Int64 :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Ok
    :<|> "add_user" :> ApiAddUser

api :: Proxy TestApi
api = Proxy

server :: Server TestApi
server =
  addPictures
    :<|> addUser

addUser :: User -> MultipartData Mem -> Handler Ok
addUser u form = do
  userIsAdmin_ u
  case lookupInput "user" form of
    Left er -> throwError err400 {errReasonPhrase = "No user info"}
    Right userPayload -> do
      let incUser = (decode . LBS.fromStrict . encodeUtf8 $ userPayload :: Maybe IncomingUser)
      case incUser of
        Nothing -> throwError err400 {errReasonPhrase = "No user info"}
        Just u -> do
          -- liftIO . runDBDev $ checkPic (avatar u) >> insertUser (incUserToDbUser u)
          return (Ok True "ok")
  where
    checkPic Nothing = return ()
    checkPic (Just imId) =
      get imId
        >>= maybe
          (lift . throwError $ err400 {errReasonPhrase = "No image with given id"})
          (\_ -> return ())
    insertUser u = insert u

addPictures :: Maybe Int64 -> MultipartData Mem -> Handler Ok
addPictures maybeArtId form = do
  let articleId = maybeArtId <|> (either (const Nothing) (readMaybe . T.unpack) . lookupInput "article_id" $ form)
      fls = files form
  case articleId of
    Nothing -> return $ Ok False "No article id provided."
    Just aId -> do
      when (length fls > maxFiles) (throwError err413) --too many files
      mapM_ validateAsImage fls
      pathes <- liftIO $ saveFiles fls
      liftIO . runDBDev $ mapM_ (\(fd, p) -> saveDb aId (T.unpack . fdFileCType $ fd) p) (zip fls pathes)
      return $ Ok True "ok"
  where
    saveDb aId mime fp = void $ insert $ Image mime fp (ArticleKey . SqlBackendKey $ aId)

validateAsImage :: FileData Mem -> Handler ()
validateAsImage fd = do
  when (T.takeWhile (/= '/') (fdFileCType fd) /= "image") (throwError err415) --not an image
  when (LBS.length (fdPayload fd) > maxImageSize) (throwError err413) --file too large

saveFiles :: [FileData Mem] -> IO [FilePath]
saveFiles fds = do
  (year, month', day') <- toGregorian . utctDay <$> getCurrentTime
  let trg = imageRoot ++ "/" ++ show year ++ "/" ++ show month' ++ "/" ++ show day'
  createDirectoryIfMissing True trg
  mapM
    ( \fd ->
        bracket
          (mkstemps (trg ++ "/" ++ "image") (dropWhile (/= '.') (T.unpack . fdFileName $ fd)))
          (hClose . snd)
          (\(fp, h) -> LBS.hPut h (fdPayload fd) >> return fp)
    )
    fds

saveFile :: FileData Mem -> IO FilePath
saveFile fd = head <$> saveFiles [fd]

-- app :: Application
-- app = serve api addPictures