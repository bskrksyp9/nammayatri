{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent where

import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType (OnUpdateEventType (RIDE_STARTED))
import qualified Control.Lens as L
import Data.Aeson as A
import Data.OpenApi hiding (Example, example)
import EulerHS.Prelude hiding (force, id, (.=))
import GHC.Exts (fromList)
import Kernel.Utils.Schema

data RideStartedEvent = RideStartedEvent
  { id :: Text,
    update_target :: Text,
    fulfillment :: FulfillmentInfo,
    force :: Maybe Bool -- FIXME find proper field
  }
  deriving (Generic, Show)

instance ToJSON RideStartedEvent where
  toJSON RideStartedEvent {..} = do
    let (A.Object fulfJSON) = toJSON fulfillment
    A.Object $
      "id" .= id
        <> "./komn/update_target" .= update_target
        <> "fulfillment" .= (fulfJSON <> ("state" .= (("code" .= RIDE_STARTED) :: A.Object)))
        <> "state" .= force

instance FromJSON RideStartedEvent where
  parseJSON = withObject "RideStartedEvent" $ \obj -> do
    update_type <- (obj .: "fulfillment") >>= (.: "state") >>= (.: "code")
    unless (update_type == RIDE_STARTED) $ fail "Wrong update_type."
    RideStartedEvent
      <$> obj .: "id"
      <*> obj .: "./komn/update_target"
      <*> obj .: "fulfillment"
      <*> obj .:? "force"

instance ToSchema RideStartedEvent where
  declareNamedSchema _ = do
    txt <- declareSchemaRef (Proxy :: Proxy Text)
    boolean <- declareSchemaRef (Proxy :: Proxy Bool)
    update_type <- declareSchemaRef (Proxy :: Proxy OnUpdateEventType)
    let st =
          mempty
            & type_ L.?~ OpenApiObject
            & properties
              L..~ fromList
                [("code", update_type)]
            & required L..~ ["code"]
        fulfillment =
          toInlinedSchema (Proxy :: Proxy FulfillmentInfo)
            & properties
              L.<>~ fromList [("state", Inline st)]
            & required L.<>~ ["state"]
    return $
      NamedSchema (Just "RideStartedEvent") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ fromList
              [ ("id", txt),
                ("./komn/update_target", txt),
                ("fulfillment", Inline fulfillment),
                ("force", boolean)
              ]
          & required L..~ ["id", "./komn/update_target", "fulfillment"]

newtype FulfillmentInfo = FulfillmentInfo
  { id :: Text -- bppRideId
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
