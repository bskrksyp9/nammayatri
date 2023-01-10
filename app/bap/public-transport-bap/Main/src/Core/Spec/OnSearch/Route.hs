module Core.Spec.OnSearch.Route where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi hiding (items)

data Route = Route
  { id :: Text,
    route_code :: Text,
    start_stop :: Text,
    end_stop :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance ToSchema Route where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions