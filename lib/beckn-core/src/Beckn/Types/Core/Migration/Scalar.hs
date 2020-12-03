module Beckn.Types.Core.Migration.Scalar (Scalar (..), Range (..)) where

import EulerHS.Prelude

data Scalar = Scalar
  { _type :: Maybe ScalarType,
    _value :: Int, -- FIXME: probably not integer
    _estimated_value :: Maybe Int,
    _computed_value :: Maybe Int,
    _range :: Maybe Range,
    _unit :: Text
  }
  deriving (Generic, Show)

data ScalarType = CONSTANT | VARIABLE
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data Range = Range
  { _min :: Int,
    _max :: Int
  }
  deriving (Generic, Show)

instance FromJSON Scalar where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Scalar where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Range where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Range where
  toJSON = genericToJSON stripAllLensPrefixOptions
