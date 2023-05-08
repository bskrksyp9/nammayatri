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

module API.Dashboard.RideBooking.Booking where

import qualified API.UI.Booking as UB
import qualified Domain.Action.UI.Booking as DBooking
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Booking.API as DB
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Servant

data RideBookingEndPoint = RideStatusEndPoint
  deriving (Show, Read)

derivePersistField "RideBookingEndPoint"

type API =
  "booking"
    :> ( CustomerBookingStatusAPI
           :<|> CustomerBookingListAPI
       )

type CustomerBookingStatusAPI =
  "ridebooking"
    :> Capture "rideBookingId" (Id SRB.Booking)
    :> Capture "customerId" (Id DP.Person)
    :> Post '[JSON] DB.BookingAPIEntity

type CustomerBookingListAPI =
  "list"
    :> Capture "customerId" (Id DP.Person)
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> QueryParam "onlyActive" Bool
    :> QueryParam "status" SRB.BookingStatus
    :> Get '[JSON] DBooking.BookingListRes

handler :: FlowServer API
handler =
  callBookingStatus
    :<|> callBookingList

callBookingStatus :: Id SRB.Booking -> Id DP.Person -> FlowHandler DB.BookingAPIEntity
callBookingStatus = UB.bookingStatus

callBookingList :: Id DP.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> FlowHandler DBooking.BookingListRes
callBookingList = UB.bookingList
