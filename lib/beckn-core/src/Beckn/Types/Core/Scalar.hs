module Beckn.Types.Core.Scalar where

import Beckn.Types.Core.ScalarRange
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Scalar = Scalar
  { _type :: Text, -- ["CONSTANT", "VARIABLE"]
    _value :: Maybe Double,
    _estimated_value :: Maybe Double,
    _computed_value :: Maybe Double,
    _range :: ScalarRange,
    _unit :: Text
  }
  deriving (Generic, Show)

instance FromJSON Scalar where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Scalar where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Scalar where
  example =
    Scalar
      { _type = "CONSTANT",
        _value = Just 12.345,
        _estimated_value = Just 12.345,
        _computed_value = Just 12.345,
        _range = example,
        _unit = "meters"
      }
