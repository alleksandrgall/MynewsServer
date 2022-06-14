module TestHandler where

import qualified App.Test as A
import qualified DB.Test as DB
import qualified Katip.Test as L
import Test.Hspec (Spec, hspec)

--Simple helper function to construct a handler for tests
withTestHandler :: (A.Handler -> Spec) -> IO ()
withTestHandler f = L.withHandler $ \lH ->
  DB.withHandler $ \dbH ->
    A.withHandler dbH lH $ \appH -> hspec $ f appH
