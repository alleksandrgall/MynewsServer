{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module DB.Operations where

import Database.Persist (LiteralType (Unescaped))
import Database.Persist.Migration

createUser :: Operation
createUser =
  CreateTable
    { name = "user",
      schema =
        [ Column "user_id" SqlInt64 [NotNull, AutoIncrement],
          Column "user_name" SqlString [NotNull],
          Column "avatar" SqlInt64 [References ("image", "image_id")],
          Column "password_hash" SqlBlob [NotNull],
          Column "created" SqlDay [NotNull, Default (PersistLiteral_ Unescaped "CURRENT_DATE")],
          Column "is_admin" SqlBool [NotNull],
          Column "is_author" SqlBool [NotNull]
        ],
      constraints =
        [ PrimaryKey ["user_id"],
          Unique "unique_user_name" ["user_name"]
        ]
    }

createCategory :: Operation
createCategory =
  CreateTable
    { name = "category",
      schema =
        [ Column "category_id" SqlInt64 [NotNull, AutoIncrement],
          Column "category_name" SqlString [NotNull],
          Column "parent_category" SqlInt64 []
        ],
      constraints =
        [PrimaryKey ["category_id"]]
    }

addCategoryParentReference :: Operation
addCategoryParentReference =
  RawOperation
    { message = "Adding reference and on delete cascade constraint to column \"parent_category\"",
      rawOp =
        return
          [ MigrateSql
              ( "ALTER TABLE category ADD CONSTRAINT category_parent_category_fkey "
                  <> "FOREIGN KEY (parent_category) REFERENCES category(category_id) ON DELETE CASCADE ON UPDATE RESTRICT;"
              )
              []
          ]
    }

createImage :: Operation
createImage =
  CreateTable
    { name = "image",
      schema =
        [ Column "image_id" SqlInt64 [NotNull, AutoIncrement],
          Column "ct" SqlString [NotNull],
          Column "path" SqlString [NotNull]
        ],
      constraints =
        [PrimaryKey ["image_id"]]
    }

createArticle :: Operation
createArticle =
  CreateTable
    { name = "article",
      schema =
        [ Column "article_id" SqlInt64 [NotNull, AutoIncrement],
          Column "title" (SqlOther "varchar(255)") [NotNull],
          Column "created" SqlDay [NotNull, Default (PersistLiteral_ Unescaped "CURRENT_DATE")],
          Column "user_id" SqlInt64 [NotNull, References ("user", "user_id")],
          Column "category_id" SqlInt64 [NotNull, References ("category", "category_id")],
          Column "content" SqlString [NotNull],
          Column "is_published" SqlBool [NotNull]
        ],
      constraints = [PrimaryKey ["article_id"]]
    }

createImageArticle :: Operation
createImageArticle =
  CreateTable
    { name = "image_article",
      schema =
        [ Column "image_article_id" SqlInt64 [NotNull, AutoIncrement],
          Column "article_id" SqlInt64 [NotNull],
          Column "image_id" SqlInt64 [NotNull]
        ],
      constraints =
        [PrimaryKey ["image_article_id"]]
    }

addImageArticleArticleReference :: Operation
addImageArticleArticleReference =
  RawOperation
    { message =
        "Adding article foreign key constraint "
          <> "and on delete cascade constraint to \"article_id\" in table \"image_article\"",
      rawOp =
        return
          [ MigrateSql
              ( "ALTER TABLE image_article "
                  <> "ADD CONSTRAINT image_article_article_id_fkey "
                  <> "FOREIGN KEY (article_id) REFERENCES article(article_id) ON DELETE CASCADE ON UPDATE RESTRICT;"
              )
              []
          ]
    }

addImageArticleImageReference :: Operation
addImageArticleImageReference =
  RawOperation
    { message =
        "Adding image foreign key constraint "
          <> "and on delete cascade constraint to \"image_id\" in table \"image_article\"",
      rawOp =
        return
          [ MigrateSql
              ( "ALTER TABLE image_article "
                  <> "ADD CONSTRAINT image_article_image_id_fkey "
                  <> "FOREIGN KEY (image_id) REFERENCES image(image_id) ON DELETE CASCADE ON UPDATE RESTRICT;"
              )
              []
          ]
    }