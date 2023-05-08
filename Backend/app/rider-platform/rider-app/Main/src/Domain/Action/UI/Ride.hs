{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride
  ( GetDriverLocResp,
    GetRideStatusResp (..),
    getDriverLoc,
    getRideStatus,
  )
where

import Domain.Types.Booking.BookingLocation (BookingLocationAPIEntity, makeBookingLocationAPIEntity)
import qualified Domain.Types.Booking.Type as DB
import qualified Domain.Types.Person as SPerson
import Domain.Types.Ride
import qualified Domain.Types.Ride as SRide
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude hiding (HasField)
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as MapSearch
import Tools.Metrics
import qualified Tools.Notifications as Notify

type GetDriverLocResp = MapSearch.LatLong

data GetRideStatusResp = GetRideStatusResp
  { fromLocation :: BookingLocationAPIEntity,
    toLocation :: Maybe BookingLocationAPIEntity,
    ride :: RideAPIEntity,
    customer :: SPerson.PersonAPIEntity,
    driverPosition :: Maybe MapSearch.LatLong
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

getDriverLoc ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    HasField "rideCfg" r RideConfig
  ) =>
  Id SRide.Ride ->
  Id SPerson.Person ->
  m GetDriverLocResp
getDriverLoc rideId personId = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  when
    (ride.status == COMPLETED || ride.status == CANCELLED)
    $ throwError $ RideInvalidStatus "Cannot track this ride"
  res <- CallBPP.callGetDriverLocation ride.trackingUrl
  booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let fromLocation = Maps.getCoordinates booking.fromLocation
  driverReachedDistance <- asks (.rideCfg.driverReachedDistance)
  driverOnTheWayNotifyExpiry <- getSeconds <$> asks (.rideCfg.driverOnTheWayNotifyExpiry)
  mbIsOnTheWayNotified <- Redis.get @() driverOnTheWay
  mbHasReachedNotified <- Redis.get @() driverHasReached
  when (ride.status == NEW && (isNothing mbIsOnTheWayNotified || isNothing mbHasReachedNotified)) $ do
    let distance = highPrecMetersToMeters $ distanceBetweenInMeters fromLocation res.currPoint
    mbStartDistance <- Redis.get @Meters distanceUpdates
    case mbStartDistance of
      Nothing -> Redis.setExp distanceUpdates distance 3600
      Just startDistance -> when (startDistance - 50 > distance) $ do
        unless (isJust mbIsOnTheWayNotified) $ do
          Notify.notifyDriverOnTheWay personId
          Redis.setExp driverOnTheWay () driverOnTheWayNotifyExpiry
        when (isNothing mbHasReachedNotified && distance <= driverReachedDistance) $ do
          Notify.notifyDriverHasReached personId ride.otp ride.vehicleNumber
          Redis.setExp driverHasReached () 1500
  return res.currPoint
  where
    distanceUpdates = "Ride:GetDriverLoc:DriverDistance " <> rideId.getId
    driverOnTheWay = "Ride:GetDriverLoc:DriverIsOnTheWay " <> rideId.getId
    driverHasReached = "Ride:GetDriverLoc:DriverHasReached " <> rideId.getId

getRideStatus ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    HasField "rideCfg" r RideConfig
  ) =>
  Id SRide.Ride ->
  Id SPerson.Person ->
  m GetRideStatusResp
getRideStatus rideId personId = withLogTag ("personId-" <> personId.getId) do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  mbPos <-
    if ride.status == COMPLETED || ride.status == CANCELLED
      then return Nothing
      else Just <$> CallBPP.callGetDriverLocation ride.trackingUrl
  booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  rider <- runInReplica $ QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  decRider <- decrypt rider
  return $
    GetRideStatusResp
      { fromLocation = makeBookingLocationAPIEntity booking.fromLocation,
        toLocation = case booking.bookingDetails of
          DB.OneWayDetails details -> Just $ makeBookingLocationAPIEntity details.toLocation
          DB.RentalDetails _ -> Nothing
          DB.OneWaySpecialZoneDetails details -> Just $ makeBookingLocationAPIEntity details.toLocation
          DB.DriverOfferDetails details -> Just $ makeBookingLocationAPIEntity details.toLocation,
        ride = makeRideAPIEntity ride,
        customer = SPerson.makePersonAPIEntity decRider,
        driverPosition = mbPos <&> (.currPoint)
      }
