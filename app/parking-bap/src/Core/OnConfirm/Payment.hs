module Core.OnConfirm.Payment where

import Beckn.Prelude
import Beckn.Utils.JSON

data PaymentStatus = PAID | NOT_PAID deriving (Generic, Eq) --TODO: WHAT ARE POSSIBLE VALUES?

instance FromJSON PaymentStatus where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentStatus where
  toJSON = genericToJSON constructorsWithHyphens

data PaymentType = PRE_FULFILLMENT | POST_FULFILLMENT deriving (Generic) --TODO: WHAT ARE POSSIBLE VALUES?

instance FromJSON PaymentType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens

data PaymentParams = PaymentParams
  { amount :: Int,
    currency :: Text,
    transaction_status :: Text,
    transaction_id :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

data Payment = Payment
  { params :: PaymentParams,
    _type :: PaymentType,
    status :: PaymentStatus,
    uri :: BaseUrl,
    tl_method :: Text
  }
  deriving (Generic)

instance FromJSON Payment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Payment where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
