{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DB.Queries (getNestCategoryByName) where

import Control.Monad (when)
import DB.Scheme
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Database.Esqueleto.Experimental hiding (isNothing)
import Types.NestCategories

{-
  Entity category deserialization into a nested type
-}
catListToNest :: [Entity Category] -> NestCategory
catListToNest ents =
  let mEnt = M.fromList $ map (\ent -> (categoryParent . entityVal $ ent, ent)) ents
      root = find (isNothing . categoryParent . entityVal) ents
      children Nothing = Non
      children (Just c) = Cat (categoryName . entityVal $ c) $ children . M.lookup (Just $ entityKey c) $ mEnt
   in children root

getNestCategoryByName :: String -> SqlPersistM NestCategory
getNestCategoryByName catName = do
  catListToNest
    <$> ( select $ do
            categories <-
              withRecursive
                ( do
                    cat <- from $ table @Category
                    where_ $ cat ^. CategoryName ==. val catName
                    pure cat
                )
                union_
                ( \self -> do
                    (cSelf :& c) <-
                      from $
                        self
                          `innerJoin` table @Category
                          `on` \(cSelf :& c) ->
                            cSelf ^. CategoryParent ==. just (c ^. CategoryId)
                    pure c
                )
            from categories
        )