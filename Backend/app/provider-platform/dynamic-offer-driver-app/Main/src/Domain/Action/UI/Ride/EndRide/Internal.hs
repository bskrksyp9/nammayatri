{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.EndRide.Internal
  ( endRideTransaction,
    putDiffMetric,
    getDistanceBetweenPoints,
  )
where

import qualified Domain.Types.Booking as SRB
import Domain.Types.Booking.TripLocation as TripLocationType
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.FareParameters as DFare
import Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RiderDetails as RD
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.FCM.Types as FCM
import Kernel.External.Maps
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverLocation as DLoc
import qualified SharedLogic.GoogleMaps as GoogleMaps
import qualified SharedLogic.Ride as SRide
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.DriverInformation as CDI
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import Storage.Queries.Booking.TripLocation as TripLocation
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.FareParameters as QFare
import Storage.Queries.Person as SQP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import Tools.Notifications (sendNotificationToDriver)

endRideTransaction ::
  (Metrics.CoreMetrics m, CacheFlow m r, EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)) =>
  Id DP.Driver ->
  Id SRB.Booking ->
  Ride.Ride ->
  Maybe DFare.FareParameters ->
  Maybe (Id RD.RiderDetails) ->
  Bool ->
  LatLong ->
  m (Maybe TripLocationType.TripLocation, Maybe TripLocationType.TripLocation)
endRideTransaction driverId bookingId ride mbFareParams mbRiderDetailsId pickupDropOutsideOfThreshold tripEndPoint = do
  (startLocationCustomer, endLocationCustomer) <-
    if pickupDropOutsideOfThreshold
      then do
        person <- Esq.runInReplica $ QPerson.findById (cast driverId) >>= fromMaybeM (PersonDoesNotExist driverId.getId)
        let merchantId = person.merchantId
        tripStartLatLong <- ride.tripStartPos & fromMaybeM LocationNotFound
        fromLocationResponse <- Maps.getTripPlaceName merchantId GetPlaceNameReq {getBy = ByLatLong LatLong {lat = tripStartLatLong.lat, lon = tripStartLatLong.lon}, sessionToken = Nothing, language = Nothing}
        actualFromLocationId <- generateGUID
        fromAddress <- GoogleMaps.mkLocation fromLocationResponse
        actualFromLocation <- buildLocation actualFromLocationId tripStartLatLong fromAddress
        toLocationResponse <- Maps.getTripPlaceName merchantId GetPlaceNameReq {getBy = ByLatLong LatLong {lat = tripEndPoint.lat, lon = tripEndPoint.lon}, sessionToken = Nothing, language = Nothing}
        actualToLocationId <- generateGUID
        toAddress <- GoogleMaps.mkLocation toLocationResponse
        actualToLocation <- buildLocation actualToLocationId tripEndPoint toAddress
        Esq.runTransaction $ do
          TripLocation.create actualFromLocation
          TripLocation.create actualToLocation
          QRide.updateLocationIds ride.id actualFromLocationId actualToLocationId
        return (Just actualFromLocation, Just actualToLocation)
      else return (Nothing, Nothing)

  driverInfo <- CDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  mbRiderDetails <- join <$> QRD.findById `mapM` mbRiderDetailsId
  minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
  let shouldUpdateRideComplete =
        case minTripDistanceForReferralCfg of
          Just distance -> (metersToHighPrecMeters <$> ride.chargeableDistance) >= Just distance && maybe True (not . (.hasTakenValidRide)) mbRiderDetails
          Nothing -> True
  let referralMessage = "Congratulations!"
  let referralTitle = "Your referred customer has completed their first Namma Yatri ride"
  when shouldUpdateRideComplete $
    fork "REFERRAL_ACTIVATED FCM to Driver" $ do
      whenJust mbRiderDetails $ \riderDetails -> do
        case riderDetails.referredByDriver of
          Just referredDriverId -> do
            driver <- SQP.findById referredDriverId >>= fromMaybeM (PersonNotFound referredDriverId.getId)
            sendNotificationToDriver driver.merchantId FCM.SHOW Nothing FCM.REFERRAL_ACTIVATED referralTitle referralMessage driver.id driver.deviceToken
          Nothing -> pure ()
  Esq.runTransaction $ do
    whenJust mbRiderDetails $ \riderDetails ->
      when shouldUpdateRideComplete (QRD.updateHasTakenValidRide riderDetails.id)
    whenJust mbFareParams QFare.create
    QRide.updateAll ride.id ride
    QRide.updateStatus ride.id Ride.COMPLETED
    QRB.updateStatus bookingId SRB.COMPLETED
    DriverStats.updateIdleTime driverId
    if driverInfo.active
      then QDFS.updateStatus ride.driverId DDFS.ACTIVE
      else QDFS.updateStatus ride.driverId DDFS.IDLE
  DLoc.updateOnRide driverId False
  SRide.clearCache $ cast driverId
  return (startLocationCustomer, endLocationCustomer)

putDiffMetric :: (Metrics.HasBPPMetrics m r, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Money -> Meters -> m ()
putDiffMetric merchantId money mtrs = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  Metrics.putFareAndDistanceDeviations org.name money mtrs

getDistanceBetweenPoints ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Metrics.CoreMetrics m,
    Maps.HasCoordinates a,
    Maps.HasCoordinates b
  ) =>
  Id Merchant ->
  a ->
  b ->
  m Meters
getDistanceBetweenPoints merchantId a b = do
  distRes <-
    Maps.getDistance merchantId $
      Maps.GetDistanceReq
        { origin = a,
          destination = b,
          travelMode = Just Maps.CAR
        }
  return $ distRes.distance

buildLocation :: (MonadFlow m) => Id TripLocationType.TripLocation -> LatLong -> GoogleMaps.Address -> m TripLocationType.TripLocation
buildLocation locationId LatLong {..} address = do
  currTime <- getCurrentTime
  return $
    TripLocationType.TripLocation
      { id = locationId,
        address =
          TripLocationType.LocationAddress
            { street = address.street,
              city = address.city,
              state = address.state,
              country = address.country,
              building = address.building,
              areaCode = address.areaCode,
              area = address.area
            },
        createdAt = currTime,
        updatedAt = currTime,
        ..
      }
