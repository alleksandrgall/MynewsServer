module TestHandler where

import qualified App.Test as A
import qualified DB.Test as DB
import qualified Katip.Test as L

withTestApp :: (A.Handler -> IO a) -> IO a
withTestApp f = L.withHandler $ \lH ->
  DB.withHandler $ \dbH ->
    A.withHandler dbH lH $ \appH -> f appH