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
import qualified SharedLogic.Ride as SRide
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.DriverInformation as CDI
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.FareParameters as QFare
import Storage.Queries.Person as SQP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import Tools.Notifications (sendNotificationToDriver)

endRideTransaction ::
  (Metrics.CoreMetrics m, CacheFlow m r, EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)) =>
  Id DP.Driver ->
  Id SRB.Booking ->
  Ride.Ride ->
  Maybe DFare.FareParameters ->
  Maybe (Id RD.RiderDetails) ->
  m ()
endRideTransaction driverId bookingId ride mbFareParams mbRiderDetailsId = do
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

putDiffMetric :: (Metrics.HasBPPMetrics m r, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Money -> Meters -> m ()
putDiffMetric merchantId money mtrs = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  Metrics.putFareAndDistanceDeviations org.name money mtrs

getDistanceBetweenPoints ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Metrics.CoreMetrics m
  ) =>
  Id Merchant ->
  LatLong ->
  LatLong ->
  [LatLong] ->
  m Meters
getDistanceBetweenPoints merchantId origin destination interpolatedPoints = do
  routeResponse <-
    Maps.getRoutes merchantId $
      Maps.GetRoutesReq
        { waypoints = origin :| ((pickWaypoints interpolatedPoints) <> [destination]),
          mode = Just Maps.CAR,
          calcPoints = True
        }
  let mbShortestRouteDistance = (.distance) =<< getRouteInfoWithShortestDuration routeResponse
  -- Next error is impossible, because we never receive empty list from directions api
  mbShortestRouteDistance & fromMaybeM (InvalidRequest "Couldn't calculate route distance")

-- TODO reuse code from rider-app
getRouteInfoWithShortestDuration :: [Maps.RouteInfo] -> Maybe Maps.RouteInfo
getRouteInfoWithShortestDuration [] = Nothing
getRouteInfoWithShortestDuration (routeInfo : routeInfoArray) =
  if null routeInfoArray
    then Just routeInfo
    else do
      restRouteResult <- getRouteInfoWithShortestDuration routeInfoArray
      Just $ comparator routeInfo restRouteResult
  where
    comparator route1 route2 =
      if route1.duration < route2.duration
        then route1
        else route2

-- for distance api we can't pick more than 10 waypoints
-- test this function
pickWaypoints :: [LatLong] -> [LatLong]
pickWaypoints waypoints = do
  let step = (length waypoints) `div` 10
  foldl (\list (n, waypoint) -> if n `mod` step == 0 then waypoint : list else list) [] $ zip [1, 2 ..] waypoints
