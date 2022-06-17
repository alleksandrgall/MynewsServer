module TestHandler where

import qualified App.Test as A
import qualified DB.Test as DB
import Image.Test (ImageTestIO)
import qualified Image.Test as I
import qualified Katip.Test as L
import Test.Hspec (Spec, hspec)

--Simple helper function to construct a handler for tests
withTestHandler :: (A.Handler ImageTestIO -> Spec) -> IO ()
withTestHandler f = L.withHandler $ \lH ->
  DB.withHandler $ \dbH ->
    I.withHandler dbH $ \imH ->
      A.withHandler dbH lH imH $ \appH -> hspec $ f appH
