{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils where

import Control.Exception (Exception, throwIO)
import Control.Monad (void)
import Crypto.KDF.BCrypt (hashPassword)
import qualified Data.ByteString as BS
import Data.Data (Proxy)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, UTCTime (utctDay), getCurrentTime)
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import Handlers.App (App)
import qualified Handlers.App as A
import Handlers.App.Auth (AuthContext)
import Handlers.DB (Handler (hRunDB))
import Handlers.DB.Scheme
import Network.HTTP.Types (Status (statusCode))
import qualified Network.Wai.Handler.Warp as Warp
import Servant (HasServer, ServerT)
import Servant.Client (ClientError (FailureResponse), ResponseF (responseStatusCode))
import Test.Hspec (expectationFailure)

withApp :: (HasServer api AuthContext) => A.Handler -> Proxy api -> ServerT api App -> (Warp.Port -> IO ()) -> IO ()
withApp h api server = Warp.testWithApplication (pure $ A.serve_ h api server)

shouldBeRightOr :: (Exception e) => String -> Either e a -> IO a
shouldBeRightOr s (Left e) = expectationFailure (s <> ": " <> show e) >> throwIO e
shouldBeRightOr _ (Right u) = return u

data ExpectedJust = ExpectedJust deriving (Show, Exception)

shouldBeJustOr :: String -> Maybe a -> IO a
shouldBeJustOr s Nothing = expectationFailure s >> throwIO ExpectedJust
shouldBeJustOr _ (Just x) = return x

clearUsers :: A.Handler -> IO ()
clearUsers h = hRunDB (A.hDBHandler h) $ do
  users <- P.selectList [UserName P.!=. "admin"] []
  let avatars = [im | Just im <- map (userAvatar . P.entityVal) users]
  P.deleteWhere [UserName P.!=. "admin"]
  P.deleteWhere [ImageId P.<-. avatars]

createTestUser :: String -> String -> Maybe Day -> Maybe ImageId -> Bool -> Bool -> IO User
createTestUser uName pass mDate ava isAdmin_ isAuthor = do
  pswdH <- (hashPassword 6 $ encodeUtf8 . T.pack $ pass :: IO BS.ByteString)
  date <- maybe (utctDay <$> getCurrentTime) return mDate
  return
    User
      { userName = uName,
        userAvatar = ava,
        userPasswordHash = pswdH,
        userCreated = date,
        userIsAdmin = isAdmin_,
        userIsAuthor = isAuthor
      }

clearCategories :: A.Handler -> IO ()
clearCategories h = hRunDB (A.hDBHandler h) $ P.deleteWhere [CategoryId P.>=. (CategoryKey . P.SqlBackendKey $ 1)]

respondsWithErr :: Int -> Either ClientError a -> IO ()
respondsWithErr errCode (Left (FailureResponse _ fResp)) = do
  if (statusCode . responseStatusCode $ fResp) == errCode
    then pure ()
    else expectationFailure $ "Server didn't respond with " <> show errCode
respondsWithErr _ (Left e) = throwIO e
respondsWithErr _ _ = expectationFailure "Server didn't respond with an error"

putTestCategoryTreeAndReturn :: A.Handler -> IO [P.Entity Category]
putTestCategoryTreeAndReturn h = do
  let cat01 = Category "cat01" Nothing
      cat02 = Category "cat02" Nothing
  cat01Ent <- hRunDB (A.hDBHandler h) (P.insertEntity cat01)
  cat02Ent <- hRunDB (A.hDBHandler h) (P.insertEntity cat02)
  let cat11 = Category "cat11" $ Just $ P.entityKey cat01Ent
      cat12 = Category "cat12" $ Just $ P.entityKey cat01Ent
      cat13 = Category "cat13" $ Just $ P.entityKey cat02Ent
  cat11Ent <- hRunDB (A.hDBHandler h) (P.insertEntity cat11)
  cat12Ent <- hRunDB (A.hDBHandler h) (P.insertEntity cat12)
  cat13Ent <- hRunDB (A.hDBHandler h) (P.insertEntity cat13)
  let cat21 = Category "cat21" $ Just $ P.entityKey cat12Ent
      cat22 = Category "cat22" $ Just $ P.entityKey cat13Ent
  cat21Ent <- hRunDB (A.hDBHandler h) (P.insertEntity cat21)
  cat22Ent <- hRunDB (A.hDBHandler h) (P.insertEntity cat22)
  let cat31 = Category "cat31" $ Just $ P.entityKey cat12Ent
  cat31Ent <- hRunDB (A.hDBHandler h) (P.insertEntity cat31)
  return [cat01Ent, cat02Ent, cat11Ent, cat12Ent, cat13Ent, cat21Ent, cat22Ent, cat31Ent]
