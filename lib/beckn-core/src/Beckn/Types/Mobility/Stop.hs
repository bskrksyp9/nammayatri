module Beckn.Types.Mobility.Stop where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Location
import Beckn.Utils.Common
import Data.Text
import Data.Time
import EulerHS.Prelude

data Stop = Stop
  { _descriptor :: Maybe Descriptor,
    _location :: Location,
    _arrival_time :: StopTime,
    _departure_time :: StopTime
  }
  deriving (Generic, Show)

instance FromJSON Stop where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Stop where
  toJSON = genericToJSON stripLensPrefixOptions

data StopTime = StopTime
  { _est :: LocalTime,
    _act :: Maybe LocalTime
  }
  deriving (Generic, Show)

instance FromJSON StopTime where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON StopTime where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example StopTime where
  example =
    StopTime
      { _est = example,
        _act = example
      }

instance Example Stop where
  example =
    Stop
      { _descriptor = example,
        _location = example,
        _arrival_time = example,
        _departure_time = example
      }
