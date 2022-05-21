{-# LANGUAGE ExistentialQuantification #-}

module Api.Internal.Optional where

import Data.Maybe (isNothing)
import Database.Persist (PersistEntity (EntityField), PersistField)
import qualified Database.Persist.Sql as P

data QParam = forall a. QParam (Maybe a)

allNothing :: [QParam] -> Bool
allNothing = all (\(QParam x) -> isNothing x)

data MaybeSetter v = forall typ. PersistField typ => MaybeSetter (EntityField v typ, Maybe typ)

setsMaybe :: [MaybeSetter v] -> [P.Update v]
setsMaybe [] = []
setsMaybe (MaybeSetter (_, Nothing) : fvs) = setsMaybe fvs
setsMaybe (MaybeSetter (f, Just v) : fvs) = (f P.=. v) : setsMaybe fvs
