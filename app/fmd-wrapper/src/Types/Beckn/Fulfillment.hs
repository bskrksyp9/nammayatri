module Types.Beckn.Fulfillment where

import Data.Aeson (withObject, (.!=), (.:))
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (State, id, state)
import Types.Beckn.FulfillmentDetails (FulfillmentDetails (..))

data Fulfillment = Fulfillment
  { id :: Text,
    tracking :: Bool,
    start :: FulfillmentDetails,
    end :: FulfillmentDetails
  }
  deriving (Generic, Show, ToJSON, ToSchema)

instance FromJSON Fulfillment where
  parseJSON = withObject "Fulfillment" $ \o ->
    Fulfillment
      <$> o .: "id"
      <*> o .: "tracking" .!= False
      <*> o .: "start"
      <*> o .: "end"
