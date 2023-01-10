module Domain.Action.UI.Ride
  ( GetDriverLocRes,
    getDriverLoc,
  )
where

import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Person as SPerson
import Domain.Types.Ride
import qualified Domain.Types.Ride as SRide
import Environment
import EulerHS.Prelude hiding (id)
import qualified SharedLogic.CallBPP as CallBPP
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as MapSearch
import Tools.Metrics
import qualified Tools.Notifications as Notify

type GetDriverLocRes = MapSearch.LatLong

getDriverLoc ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    CoreMetrics m,
    HasField "rideCfg" r RideConfig
  ) =>
  Id SRide.Ride ->
  Id SPerson.Person ->
  m GetDriverLocRes
getDriverLoc rideId personId = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  when
    (ride.status == COMPLETED || ride.status == CANCELLED)
    $ throwError $ RideInvalidStatus "Cannot track this ride"
  res <- CallBPP.callGetDriverLocation ride
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let fromLocation = booking.fromLocation
  driverReachedDistance <- asks (.rideCfg.driverReachedDistance)
  driverOnTheWayNotifyExpiry <- getSeconds <$> asks (.rideCfg.driverOnTheWayNotifyExpiry)
  mbIsOnTheWayNotified <- Redis.get @() (driverOnTheWay rideId)
  mbHasReachedNotified <- Redis.get @() (driverHasReached rideId)
  when (ride.status == NEW && (isNothing mbIsOnTheWayNotified || isNothing mbHasReachedNotified)) $ do
    distance <-
      (.distance)
        <$> MapSearch.getDistance booking.merchantId
          MapSearch.GetDistanceReq
            { origin = fromLocation,
              destination = res.currPoint,
              travelMode = Just MapSearch.CAR
            }
    mbStartDistance <- Redis.get @Meters (distanceUpdates rideId)
    case mbStartDistance of
      Nothing -> Redis.setExp (distanceUpdates rideId) distance 3600
      Just startDistance -> when (startDistance - 100 > distance) $ do
        unless (isJust mbIsOnTheWayNotified) $ do
          Notify.notifyDriverOnTheWay personId
          Redis.setExp (driverOnTheWay rideId) () driverOnTheWayNotifyExpiry
        when (isNothing mbHasReachedNotified && distance <= driverReachedDistance) $ do
          Notify.notifyDriverHasReached personId ride
          Redis.setExp (driverHasReached rideId) () 1500
  return res.currPoint

distanceUpdates :: Id SRide.Ride -> Text
distanceUpdates (Id rideId) = "BAP: DriverDistance " <> rideId

driverOnTheWay :: Id SRide.Ride -> Text
driverOnTheWay (Id rideId) = "BAP: DriverIsOnTheWay " <> rideId

driverHasReached :: Id SRide.Ride -> Text
driverHasReached (Id rideId) = "BAP: DriverHasReached " <> rideId