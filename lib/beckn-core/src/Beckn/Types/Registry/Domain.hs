module Beckn.Types.Registry.Domain (Domain (..)) where

import Beckn.Utils.JSON (constructorsWithHyphensUntagged)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Domain
  = MOBILITY
  | FINAL_MILE_DELIVERY
  | LOCAL_RETAIL
  | FOOD_AND_BEVERAGE
  | HEALTHCARE
  | METRO
  | UNKNOWN_DOMAIN Text
  deriving (Eq, Generic, Show, ToSchema)

instance ToJSON Domain where
  toJSON MOBILITY = String "nic2004:60221"
  toJSON METRO = String "nic2004:60212"
  toJSON LOCAL_RETAIL = String "nic2004:52110"
  toJSON FINAL_MILE_DELIVERY = String "nic2004:55204"
  toJSON (UNKNOWN_DOMAIN domain) = String domain
  toJSON val = genericToJSON constructorsWithHyphensUntagged val -- TODO: update remaining domains with codes

instance FromJSON Domain where
  parseJSON (String "nic2004:60221") = pure MOBILITY
  parseJSON (String "nic2004:52110") = pure LOCAL_RETAIL
  parseJSON (String "nic2004:55204") = pure FINAL_MILE_DELIVERY
  parseJSON (String "FOOD-AND-BEVERAGE") = pure FOOD_AND_BEVERAGE
  parseJSON (String "HEALTHCARE") = pure HEALTHCARE
  parseJSON (String "nic2004:60212") = pure METRO
  parseJSON (String domain) = pure $ UNKNOWN_DOMAIN domain
  parseJSON e = typeMismatch "Registry Domain" e
