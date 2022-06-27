module Internal.TestHandler where

import qualified Internal.App.Test as A
import qualified Internal.DB.Test as DB
import qualified Internal.Image.Test as I
import qualified Internal.Katip.Test as L
import Test.Hspec (Spec, hspec)

--Simple helper function to construct a handler for tests
withTestHandler :: (A.Handler I.ImageTestIO -> Spec) -> IO ()
withTestHandler f = L.withHandler $ \lH ->
  DB.withHandler $ \dbH ->
    I.withHandler dbH $ \imH ->
      A.withHandler dbH lH imH $ \appH -> hspec $ f appH
