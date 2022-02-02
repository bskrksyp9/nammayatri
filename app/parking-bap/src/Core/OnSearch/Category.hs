module Core.OnSearch.Category (Category (..)) where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Category = Category
  { id :: Maybe Text,
    descriptor :: Maybe CategoryDescriptor
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype CategoryDescriptor = CategoryDescriptor
  { name :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToSchema CategoryDescriptor where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions