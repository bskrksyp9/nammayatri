{-# LANGUAGE DerivingVia #-}

module SharedLogic.DriverPool
  ( calculateDriverPool,
    calculateDriverPoolWithActualDist,
    incrementTotalQuotesCount,
    incrementQuoteAcceptedCount,
    getLatestAcceptanceRatio,
    incrementTotalRidesCount,
    incrementCancellationCount,
    getLatestCancellationRatio,
    getCurrentWindowAvailability,
    module Reexport,
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Id
import qualified Beckn.Types.SlidingWindowCounters as SWC
import Beckn.Utils.Common
import qualified Beckn.Utils.SlidingWindowCounters as SWC
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Domain.Types.Vehicle.Variant (Variant)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import SharedLogic.DriverPool.Config as Reexport
import SharedLogic.DriverPool.Types as Reexport
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Person as QP
import Tools.Maps as Maps
import Tools.Metrics

mkTotalQuotesKey :: Text -> Text
mkTotalQuotesKey driverId = "driver-offer:DriverPool:Total-quotes:DriverId-" <> driverId

mkQuotesAcceptedKey :: Text -> Text
mkQuotesAcceptedKey driverId = "driver-offer:DriverPool:Quote-accepted:DriverId-" <> driverId

mkTotalRidesKey :: Text -> Text
mkTotalRidesKey driverId = "driver-offer:DriverPool:Total-Rides:DriverId-" <> driverId

mkRideCancelledKey :: Text -> Text
mkRideCancelledKey driverId = "driver-offer:DriverPool:Ride-cancelled:DriverId-" <> driverId

mkAvailableTimeKey :: Text -> Text
mkAvailableTimeKey driverId = "driver-offer:DriverPool:Available-time:DriverId-" <> driverId

withWindowOptions ::
  ( Redis.HedisFlow m r,
    SWC.HasWindowOptions r,
    L.MonadFlow m
  ) =>
  (SWC.SlidingWindowOptions -> m a) ->
  m a
withWindowOptions fn = do
  asks (.windowOptions) >>= fn

incrementTotalQuotesCount ::
  ( Redis.HedisFlow m r,
    SWC.HasWindowOptions r,
    L.MonadFlow m
  ) =>
  Id DP.Person ->
  m ()
incrementTotalQuotesCount driverId = Redis.withCrossAppRedis $ withWindowOptions $ SWC.incrementWindowCount (mkTotalQuotesKey driverId.getId)

incrementQuoteAcceptedCount ::
  ( Redis.HedisFlow m r,
    SWC.HasWindowOptions r,
    L.MonadFlow m
  ) =>
  Id DP.Person ->
  m ()
incrementQuoteAcceptedCount driverId = Redis.withCrossAppRedis $ withWindowOptions $ SWC.incrementWindowCount (mkQuotesAcceptedKey driverId.getId)

getLatestAcceptanceRatio ::
  ( L.MonadFlow m,
    SWC.HasWindowOptions r,
    Redis.HedisFlow m r
  ) =>
  Id DP.Driver ->
  m Double
getLatestAcceptanceRatio driverId = Redis.withCrossAppRedis $ withWindowOptions $ SWC.getLatestRatio (getId driverId) mkQuotesAcceptedKey mkTotalQuotesKey

incrementTotalRidesCount ::
  ( Redis.HedisFlow m r,
    SWC.HasWindowOptions r,
    L.MonadFlow m
  ) =>
  Id DP.Person ->
  m ()
incrementTotalRidesCount driverId = Redis.withCrossAppRedis $ withWindowOptions $ SWC.incrementWindowCount (mkTotalRidesKey driverId.getId)

incrementCancellationCount ::
  ( Redis.HedisFlow m r,
    SWC.HasWindowOptions r,
    L.MonadFlow m
  ) =>
  Id DP.Person ->
  m ()
incrementCancellationCount driverId = Redis.withCrossAppRedis $ withWindowOptions $ SWC.incrementWindowCount (mkRideCancelledKey driverId.getId)

getLatestCancellationRatio ::
  ( L.MonadFlow m,
    SWC.HasWindowOptions r,
    Redis.HedisFlow m r
  ) =>
  Id DP.Driver ->
  m Double
getLatestCancellationRatio driverId = Redis.withCrossAppRedis $ withWindowOptions $ SWC.getLatestRatio driverId.getId mkRideCancelledKey mkTotalRidesKey

getCurrentWindowAvailability ::
  ( L.MonadFlow m,
    SWC.HasWindowOptions r,
    Redis.HedisFlow m r,
    FromJSON a
  ) =>
  Id DP.Driver ->
  m [Maybe a]
getCurrentWindowAvailability driverId = Redis.withCrossAppRedis $ withWindowOptions $ SWC.getCurrentWindowValues (mkAvailableTimeKey driverId.getId)

calculateDriverPool ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasCoordinates a
  ) =>
  DriverPoolConfig ->
  Maybe Variant ->
  a ->
  Id DM.Merchant ->
  Bool ->
  Maybe PoolRadiusStep ->
  m [DriverPoolResult]
calculateDriverPool driverPoolCfg mbVariant pickup merchantId onlyNotOnRide mRadiusStep = do
  let radius = getRadius mRadiusStep
  let coord = getCoordinates pickup
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      Esq.runInReplica $
        QP.getNearestDrivers
          mbVariant
          coord
          radius
          merchantId
          onlyNotOnRide
          driverPoolCfg.driverPositionInfoExpiry
  return $ makeDriverPoolResult <$> approxDriverPool
  where
    getRadius mRadiusStep_ = do
      let maxRadius = fromIntegral driverPoolCfg.maxRadiusOfSearch
      case mRadiusStep_ of
        Just radiusStep -> do
          let minRadius = fromIntegral driverPoolCfg.minRadiusOfSearch
          let radiusStepSize = fromIntegral driverPoolCfg.radiusStepSize
          min (minRadius + radiusStepSize * radiusStep) maxRadius
        Nothing -> maxRadius
    makeDriverPoolResult :: QP.NearestDriversResult -> DriverPoolResult
    makeDriverPoolResult QP.NearestDriversResult {..} = do
      DriverPoolResult
        { distanceToPickup = distanceToDriver,
          variant = variant,
          ..
        }

calculateDriverPoolWithActualDist ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasCoordinates a
  ) =>
  DriverPoolConfig ->
  Maybe Variant ->
  a ->
  Id DM.Merchant ->
  Bool ->
  Maybe PoolRadiusStep ->
  m [DriverPoolWithActualDistResult]
calculateDriverPoolWithActualDist driverPoolCfg mbVariant pickup merchantId onlyNotOnRide mRadiusStep = do
  driverPool <- calculateDriverPool driverPoolCfg mbVariant pickup merchantId onlyNotOnRide mRadiusStep
  case driverPool of
    [] -> return []
    (a : pprox) -> do
      driverPoolWithActualDist <- computeActualDistance merchantId pickup (a :| pprox)
      let filtDriverPoolWithActualDist = case driverPoolCfg.actualDistanceThreshold of
            Nothing -> NE.toList driverPoolWithActualDist
            Just threshold -> NE.filter (filterFunc threshold) driverPoolWithActualDist
      logDebug $ "secondly filtered driver pool" <> show filtDriverPoolWithActualDist
      return filtDriverPoolWithActualDist
  where
    filterFunc threshold estDist = getMeters estDist.actualDistanceToPickup <= fromIntegral threshold

computeActualDistance ::
  ( CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasCoordinates a
  ) =>
  Id DM.Merchant ->
  a ->
  NonEmpty DriverPoolResult ->
  m (NonEmpty DriverPoolWithActualDistResult)
computeActualDistance orgId pickup driverPoolResults = do
  let pickupLatLong = getCoordinates pickup
  getDistanceResults <-
    Maps.getDistances orgId $
      Maps.GetDistancesReq
        { origins = driverPoolResults,
          destinations = pickupLatLong :| [],
          travelMode = Just Maps.CAR
        }
  logDebug $ "get distance results" <> show getDistanceResults
  return $ mkDriverPoolWithActualDistResult <$> getDistanceResults
  where
    mkDriverPoolWithActualDistResult distDur = do
      DriverPoolWithActualDistResult
        { driverPoolResult = distDur.origin,
          actualDistanceToPickup = distDur.distance,
          actualDurationToPickup = distDur.duration
        }