module Beckn.Types.Core.Metro.Search.Location where

import Beckn.Types.Core.Metro.Search.Address (Address)
import Beckn.Types.Core.Metro.Search.Circle (Circle)
import Beckn.Types.Core.Metro.Search.City (City)
import Beckn.Types.Core.Metro.Search.Country (Country)
import Beckn.Types.Core.Metro.Search.Descriptor (Descriptor)
import Beckn.Types.Core.Metro.Search.Gps (Gps)
import Beckn.Types.Core.Metro.Search.Time (Time)
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data Location = Location
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    gps :: Maybe Gps,
    address :: Maybe Address,
    station_code :: Maybe Text,
    city :: Maybe City,
    country :: Maybe Country,
    circle :: Maybe Circle,
    polygon :: Maybe Text,
    _3dspace :: Maybe Text,
    time :: Maybe Time
  }
  deriving (Generic, Show, ToSchema)

emptyLocation :: Location
emptyLocation =
  Location
    { id = Nothing,
      descriptor = Nothing,
      gps = Nothing,
      address = Nothing,
      station_code = Nothing,
      city = Nothing,
      country = Nothing,
      circle = Nothing,
      polygon = Nothing,
      _3dspace = Nothing,
      time = Nothing
    }

instance Example Location where
  example =
    Location
      { id = Nothing,
        descriptor = Nothing,
        gps = Nothing,
        address = Nothing,
        station_code = Nothing,
        city = Nothing,
        country = Nothing,
        circle = Nothing,
        polygon = Nothing,
        _3dspace = Nothing,
        time = Nothing
      }

instance FromJSON Location where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Location where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny