module Types.CoreMetro.Catalog where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Utils.JSON (slashedRecordFields)
import EulerHS.Prelude hiding (exp, id)
import Types.CoreMetro.Provider
import Data.OpenApi (ToSchema)

data Catalog = Catalog
  { bpp_descriptor :: Descriptor,
    bpp_providers :: [Provider]
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
