{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.Common.Booking
  ( module Dashboard.Common.Booking,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.CancellationReasons.Types
import Dashboard.Common as Reexport
import Data.Aeson
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Servant hiding (Summary)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data BookingEndpoint
  = StuckBookingsCancelEndpoint
  deriving (Show, Read)

derivePersistField "BookingEndpoint"

---------------------------------------------------------
-- bookings cancel --------------------------------------

type StuckBookingsCancelAPI =
  "cancel"
    :> "allStuck"
    :> ReqBody '[JSON] StuckBookingsCancelReq
    :> Post '[JSON] StuckBookingsCancelRes

newtype StuckBookingsCancelReq = StuckBookingsCancelReq
  { bookingIds :: [Id Booking]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype StuckBookingsCancelRes = StuckBookingsCancelRes
  { cancelledBookings :: [StuckBookingItem]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StuckBookingItem = StuckBookingItem
  { bookingId :: Id Booking,
    rideId :: Maybe (Id Ride)
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

bookingStuckCode :: CancellationReasonCode
bookingStuckCode = CancellationReasonCode "BOOKING_NEW_STATUS_MORE_THAN_6HRS"

rideStuckCode :: CancellationReasonCode
rideStuckCode = CancellationReasonCode "RIDE_NEW_STATUS_MORE_THAN_6HRS"

instance HideSecrets StuckBookingsCancelReq where
  hideSecrets = identity

instance HideSecrets StuckBookingsCancelRes where
  hideSecrets = identity
