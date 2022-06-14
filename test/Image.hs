module Image where

import Api.Image
import Handlers.DB.Scheme
import Servant.Client

getI :: ImageId -> ClientM WithCT
getI = client imageApi