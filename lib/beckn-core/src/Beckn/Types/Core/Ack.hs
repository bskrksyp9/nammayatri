module Beckn.Types.Core.Ack where

import Data.Text
import EulerHS.Prelude

newtype Ack = Ack
  { _status :: Text
  }
  deriving (Generic, Show)

instance FromJSON Ack where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Ack where
  toJSON = genericToJSON stripAllLensPrefixOptions
