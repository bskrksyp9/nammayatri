module Tools.Schema where

import Data.OpenApi
import qualified Tools.JSON as J

--TODO reuse code from app-backend
fareProductSchemaOptions :: SchemaOptions
fareProductSchemaOptions =
  defaultSchemaOptions
    { sumEncoding = J.fareProductTaggedObject,
      constructorTagModifier = J.fareProductConstructorModifier
    }