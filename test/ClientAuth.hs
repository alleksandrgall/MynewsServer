{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module ClientAuth where

import Data.ByteString (ByteString)
import Servant (AuthProtect, BasicAuthData (BasicAuthData))
import Servant.Client.Core (AuthClientData, AuthenticatedRequest, basicAuthReq, mkAuthenticatedRequest)
import Servant.Client.Core.Request (Request)

type instance AuthClientData (AuthProtect "admin") = (ByteString, ByteString)

type instance AuthClientData (AuthProtect "normal") = (ByteString, ByteString)

basicAuthRequest :: (ByteString, ByteString) -> Request -> Request
basicAuthRequest (name, pass) = basicAuthReq $ BasicAuthData name pass

authenticateAdmin :: (ByteString, ByteString) -> AuthenticatedRequest (AuthProtect "admin")
authenticateAdmin credentials = mkAuthenticatedRequest credentials basicAuthRequest

authenticateNormal :: (ByteString, ByteString) -> AuthenticatedRequest (AuthProtect "normal")
authenticateNormal credentials = mkAuthenticatedRequest credentials basicAuthRequest
