module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent as Reexport
import Beckn.Utils.Schema (untaggedValue)
import Data.OpenApi
import EulerHS.Prelude hiding ((.=))

data OnUpdateEvent
  = RideAssigned RideAssignedEvent
  | RideStarted RideStartedEvent
  | RideCompleted RideCompletedEvent
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema OnUpdateEvent where
  declareNamedSchema = genericDeclareNamedSchema untaggedValue
