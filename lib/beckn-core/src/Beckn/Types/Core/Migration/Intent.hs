module Beckn.Types.Core.Migration.Intent (Intent (..)) where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Migration.Location (Location)
import Beckn.Types.Core.Migration.Payment (PaymentType)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Types.Core.Migration.Time (Time)
import EulerHS.Prelude

data Intent = Intent
  { _query_string :: Maybe Text,
    _provider :: Maybe ProviderInfo,
    _fulfillment :: Maybe FulFillmentInfo,
    _payment :: Maybe PaymentInfo,
    _category :: Maybe CategoryInfo,
    _offer :: Maybe OfferInfo,
    _item :: Maybe ItemInfo,
    _purpose :: Maybe Text,
    _tags :: [Tags] -- Fix after that https://github.com/beckn/protocol-specifications/pull/61
  }
  deriving (Generic, Show)

data ProviderInfo = ProviderInfo
  { _id :: Maybe Text,
    _descriptor :: Maybe DescriptorName,
    _locations :: [IdObject]
  }
  deriving (Generic, Show)

newtype DescriptorName = DescriptorName {_name :: Text}
  deriving (Generic, Show)

data FulFillmentInfo = FulFillmentInfo
  { _id :: Maybe Text,
    _start :: Maybe LocationAndTime,
    _end :: Maybe LocationAndTime,
    _tags :: Tags
  }
  deriving (Generic, Show)

data LocationAndTime = LocationAndTime
  { _location :: Maybe Location,
    _time :: Maybe Time
  }
  deriving (Generic, Show)

data PaymentInfo = PaymentInfo
  { _type :: Maybe PaymentType,
    _params :: Maybe PaymentParams
  }
  deriving (Generic, Show)

newtype PaymentParams = PaymentParams
  {_mode :: Maybe Text}
  deriving (Generic, Show)

data CategoryInfo = CategoryInfo
  { _id :: Maybe Text,
    _descriptor :: Maybe DescriptorName
  }
  deriving (Generic, Show)

data OfferInfo = OfferInfo
  { _id :: Maybe Text,
    _descriptor :: Maybe DescriptorName
  }
  deriving (Generic, Show)

data ItemInfo = ItemInfo
  { _id :: Maybe Text,
    _descriptor :: Maybe ItemInfoDescriptor
  }
  deriving (Generic, Show)

data ItemInfoDescriptor = ItemInfoDescriptor
  { _name :: Maybe Text,
    _tags :: Maybe Tags
  }
  deriving (Generic, Show)

{- possible solution to this multitude of types:
data IdAndDescriptor descriptor = IdAndDescriptor
  { _id :: Maybe Text,
    _descriptor :: Maybe descriptor
  }
-}

instance FromJSON PaymentParams where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON PaymentParams where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON PaymentInfo where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON PaymentInfo where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON Intent where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Intent where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON ProviderInfo where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON ProviderInfo where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON FulFillmentInfo where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON FulFillmentInfo where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON LocationAndTime where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON LocationAndTime where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON CategoryInfo where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CategoryInfo where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON DescriptorName where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON DescriptorName where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON OfferInfo where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON OfferInfo where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON ItemInfo where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON ItemInfo where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON ItemInfoDescriptor where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON ItemInfoDescriptor where
  toJSON = genericToJSON stripLensPrefixOptions
