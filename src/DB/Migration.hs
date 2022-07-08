module DB.Migration where

import DB.Operations
import Database.Persist.Migration

migration :: Migration
migration =
  [ 0 ~> 1 := [createImage, createUser, createCategory, createArticle, createImageArticle],
    1 ~> 2 := [addCategoryParentReference],
    2 ~> 3 := [addImageArticleImageReference, addImageArticleArticleReference]
  ]
