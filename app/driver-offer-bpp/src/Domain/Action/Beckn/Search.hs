module Domain.Action.Beckn.Search where

import Beckn.Prelude
import Beckn.Serviceability
import Beckn.Storage.Hedis
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.List
import Domain.Types.FarePolicy (FarePolicy)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.Vehicle.Variant as Variant
import Environment
import qualified SharedLogic.CacheDistance as CD
import SharedLogic.DriverPool
import SharedLogic.FareCalculator
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy as FarePolicyS
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Geometry as QGeometry
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.Metrics.ARDUBPPMetrics as Metrics

data DSearchReq = DSearchReq
  { messageId :: Text,
    transactionId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: DLoc.SearchReqLocationAPIEntity,
    pickupTime :: UTCTime,
    dropLocation :: DLoc.SearchReqLocationAPIEntity
  }

data DSearchRes = DSearchRes
  { provider :: DM.Merchant,
    fromLocation :: DLoc.SearchReqLocation,
    toLocation :: DLoc.SearchReqLocation,
    now :: UTCTime,
    estimateList :: [EstimateItem],
    searchMetricsMVar :: Metrics.SearchMetricsMVar
  }

data EstimateItem = EstimateItem
  { vehicleVariant :: Variant.Variant,
    distanceToPickup :: Meters,
    minFare :: Money,
    maxFare :: Money,
    estimateBreakupList :: [BreakupItem]
  }

data TransporterInfo = TransporterInfo
  { shortId :: ShortId DM.Merchant,
    name :: Text,
    contacts :: Text,
    ridesInProgress :: Int,
    ridesCompleted :: Int,
    ridesConfirmed :: Int
  }

data BreakupItem = BreakupItem
  { title :: Text,
    price :: BreakupPrice
  }

data BreakupPrice = BreakupPrice
  { currency :: Text,
    value :: Money
  }

handler :: Id DM.Merchant -> DSearchReq -> Flow DSearchRes
handler merchantId sReq = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless org.enabled $ throwError AgencyDisabled
  searchMetricsMVar <- Metrics.startSearchMetrics org.name
  fromLocation <- buildSearchReqLocation sReq.pickupLocation
  toLocation <- buildSearchReqLocation sReq.dropLocation
  let pickupLatLong = Maps.getCoordinates fromLocation
  let mbDropoffLatLong = Just $ Maps.getCoordinates toLocation
  unlessM (rideServiceable org.geofencingConfig QGeometry.someGeometriesContain pickupLatLong mbDropoffLatLong) $
    throwError RideNotServiceable

  distRes <-
    Maps.getDistance merchantId $
      Maps.GetDistanceReq
        { origin = fromLocation,
          destination = toLocation,
          travelMode = Just Maps.CAR
        }
  let distance = distRes.distance
  let duration = distRes.duration
  CD.cacheDistance sReq.transactionId (distance, duration)
  logDebug $ "distance: " <> show distance

  allFarePolicies <- FarePolicyS.findAllByMerchantId org.id
  let farePolicies = filter (checkTripConstraints distance) allFarePolicies

  estimates <-
    if null farePolicies
      then do
        logDebug "Trip doesnot match any fare policy constraints."
        return []
      else do
        driverPool <- calculateDriverPool Nothing pickupLatLong org.id True Nothing

        logDebug $ "Search handler: driver pool " <> show driverPool

        let listOfProtoQuotes = nubBy ((==) `on` (.variant)) driverPool
            filteredProtoQuotes = zipMatched farePolicies listOfProtoQuotes

        estimates <- mapM (mkEstimate org sReq.pickupTime distance) filteredProtoQuotes
        logDebug $ "bap uri: " <> show sReq.bapUri
        return estimates

  buildSearchRes org fromLocation toLocation estimates searchMetricsMVar
  where
    checkTripConstraints tripDistance fp =
      let cond1 = (<= tripDistance) <$> fp.minAllowedTripDistance
          cond2 = (>= tripDistance) <$> fp.maxAllowedTripDistance
       in and $ catMaybes [cond1, cond2]

    zipMatched farePolicies driverPool =
      mapMaybe match farePolicies
      where
        match :: FarePolicy -> Maybe (FarePolicy, DriverPoolResult)
        match farePolicy =
          let fpVehicleVariant = farePolicy.vehicleVariant
              driverPoolVariants = map (.variant) driverPool
              midx = elemIndex fpVehicleVariant driverPoolVariants
           in fmap (\idx -> (farePolicy, driverPool !! idx)) midx

mkEstimate ::
  (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) =>
  DM.Merchant ->
  UTCTime ->
  Meters ->
  (FarePolicy, DriverPoolResult) ->
  m EstimateItem
mkEstimate org startTime dist (farePolicy, driverMetadata) = do
  fareParams <- calculateFare org.id farePolicy dist startTime Nothing
  let baseFare = fareSum fareParams
      currency = "INR"
      estimateBreakups = mkBreakupListItems (BreakupPrice currency) BreakupItem farePolicy
  logDebug $ "baseFare: " <> show baseFare
  logDebug $ "distanceToPickup: " <> show driverMetadata.distanceToPickup
  pure
    EstimateItem
      { vehicleVariant = driverMetadata.variant,
        distanceToPickup = driverMetadata.distanceToPickup,
        minFare = baseFare,
        maxFare = baseFare + farePolicy.driverExtraFee.maxFee,
        estimateBreakupList = estimateBreakups
      }

mkBreakupListItems ::
  (Money -> breakupItemPrice) ->
  (Text -> breakupItemPrice -> breakupItem) ->
  FarePolicy ->
  [breakupItem]
mkBreakupListItems mkPrice mkBreakupItem farePolicy = do
  let baseDistanceFare = roundToIntegral farePolicy.baseDistanceFare
      baseDistanceFareCaption = "BASE_DISTANCE_FARE"
      baseDistanceFareItem = mkBreakupItem baseDistanceFareCaption (mkPrice baseDistanceFare)

      perExtraKmFare = roundToIntegral farePolicy.perExtraKmFare
      perExtraKmFareCaption = "EXTRA_PER_KM_FARE"
      perExtraKmFareItem = mkBreakupItem perExtraKmFareCaption (mkPrice perExtraKmFare)

      deadKmFare = farePolicy.deadKmFare
      deadKmFareCaption = "DEAD_KILOMETER_FARE"
      deadKmFareItem = mkBreakupItem deadKmFareCaption (mkPrice deadKmFare)

      nightShiftRate = fromMaybe 1 (roundToIntegral <$> farePolicy.nightShiftRate)
      nightShiftRateCaption = "NIGH_SHIFT_FARE"
      nightShiftRateItem = mkBreakupItem nightShiftRateCaption (mkPrice nightShiftRate)

      driverMinExtraFee = farePolicy.driverExtraFee.minFee
      driverMinExtraFeeCaption = "DRIVER_MIN_EXTRA_FEE"
      driverMinExtraFeeItem = mkBreakupItem driverMinExtraFeeCaption (mkPrice driverMinExtraFee)

      driverMaxExtraFee = farePolicy.driverExtraFee.minFee
      driverMaxExtraFeeCaption = "DRIVER_MAX_EXTRA_FEE"
      driverMaxExtraFeeItem = mkBreakupItem driverMaxExtraFeeCaption (mkPrice driverMaxExtraFee)

  [baseDistanceFareItem, perExtraKmFareItem, deadKmFareItem, nightShiftRateItem, driverMinExtraFeeItem, driverMaxExtraFeeItem]

buildSearchReqLocation :: (MonadGuid m, MonadTime m) => DLoc.SearchReqLocationAPIEntity -> m DLoc.SearchReqLocation
buildSearchReqLocation DLoc.SearchReqLocationAPIEntity {..} = do
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DLoc.SearchReqLocation {..}

buildSearchRes ::
  (MonadTime m) =>
  DM.Merchant ->
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  [EstimateItem] ->
  Metrics.SearchMetricsMVar ->
  m DSearchRes
buildSearchRes org fromLocation toLocation estimateList searchMetricsMVar = do
  now <- getCurrentTime
  pure $
    DSearchRes
      { provider = org,
        now,
        fromLocation,
        toLocation,
        estimateList,
        searchMetricsMVar
      }
