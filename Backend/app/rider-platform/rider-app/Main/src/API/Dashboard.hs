{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard where

import qualified API.Dashboard.Booking as Booking
import qualified API.Dashboard.Customer as Customer
import qualified API.Dashboard.Exotel as Exotel
import qualified API.Dashboard.Merchant as Merchant
import qualified API.Dashboard.MultipleRideCancel as MultipleRideCancel
import qualified API.Dashboard.MultipleRideEnd as MultipleRideEnd
import qualified API.Dashboard.Ride as Ride
import qualified API.Dashboard.RideBooking as RideBookings
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Types.Id
import Servant hiding (throwError)
import Tools.Auth (DashboardTokenAuth)

type API =
  "dashboard"
    :> ( Capture "merchantId" (ShortId DM.Merchant)
           :> API'
       )
    :<|> ExotelAPI

type API' =
  DashboardTokenAuth
    :> ( Customer.API
           :<|> Booking.API
           :<|> Merchant.API
           :<|> Ride.API
           :<|> RideBookings.API
           :<|> MultipleRideCancel.API
           :<|> MultipleRideEnd.API
       )

handler :: FlowServer API
handler =
  ( \merchantId _dashboard ->
      Customer.handler merchantId
        :<|> Booking.handler merchantId
        :<|> Merchant.handler merchantId
        :<|> Ride.handler merchantId
        :<|> RideBookings.handler merchantId
        :<|> MultipleRideCancel.handler
        :<|> MultipleRideEnd.handler
  )
    :<|> exotelHandler

type ExotelAPI =
  DashboardTokenAuth
    :> Exotel.API

exotelHandler :: FlowServer ExotelAPI
exotelHandler _dashboard =
  Exotel.handler
