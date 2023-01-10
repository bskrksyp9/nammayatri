module SharedLogic.FareCalculator.OneWayFareCalculator.Flow
  ( OneWayFareParameters,
    ServiceHandle (..),
    calculateFare,
    doCalculateFare,
    fareSum,
    fareSumWithDiscount,
    buildOneWayFareBreakups,
  )
where

import Beckn.Prelude
import Beckn.Storage.Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Booking
import Domain.Types.FarePolicy.FareBreakup
import Domain.Types.FarePolicy.OneWayFarePolicy (OneWayFarePolicy)
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (id)
import SharedLogic.FareCalculator.OneWayFareCalculator.Calculator
  ( OneWayFareParameters (..),
    TripEndTime,
    calculateFareParameters,
    fareSum,
    fareSumWithDiscount,
  )
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.OneWayFarePolicy as OWFarePolicy
import Tools.Error

type MonadHandler m = (MonadThrow m, Log m)

newtype ServiceHandle m = ServiceHandle
  { getFarePolicy :: Id Merchant -> Vehicle.Variant -> m (Maybe OneWayFarePolicy)
  }

serviceHandle :: (CacheFlow m r, EsqDBFlow m r) => ServiceHandle m
serviceHandle =
  ServiceHandle
    { getFarePolicy = \merchantId vehicleVariant -> do
        OWFarePolicy.findByMerchantIdAndVariant merchantId vehicleVariant
    }

calculateFare ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  Id Merchant ->
  Vehicle.Variant ->
  Meters ->
  UTCTime ->
  m OneWayFareParameters
calculateFare = doCalculateFare serviceHandle

doCalculateFare ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Merchant ->
  Vehicle.Variant ->
  Meters ->
  TripEndTime ->
  m OneWayFareParameters
doCalculateFare ServiceHandle {..} merchantId vehicleVariant distance endTime = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| merchantId ||+ " for " +|| vehicleVariant ||+ ""
  farePolicy <- getFarePolicy merchantId vehicleVariant >>= fromMaybeM NoFarePolicy
  let fareParams = calculateFareParameters farePolicy distance endTime
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams

buildOneWayFareBreakups :: MonadGuid m => OneWayFareParameters -> Id Booking -> m [FareBreakup]
buildOneWayFareBreakups fareParams bookingId = do
  baseFareBreakup <- buildBaseFareBreakup fareParams bookingId
  distanceFareBreakup <- buildDistanceFareBreakup fareParams bookingId
  discountFareBreakup <- buildDiscountFareBreakup fareParams.discount bookingId
  pure $ [baseFareBreakup, distanceFareBreakup] <> maybeToList discountFareBreakup

buildBaseFareBreakup :: MonadGuid m => OneWayFareParameters -> Id Booking -> m FareBreakup
buildBaseFareBreakup OneWayFareParameters {..} bookingId = do
  id <- Id <$> generateGUIDText
  let amount = nightShiftRate * fromIntegral baseFare
      description = "Base fare is " <> show amount <> " rupees"
  pure FareBreakup {..}

buildDistanceFareBreakup :: MonadGuid m => OneWayFareParameters -> Id Booking -> m FareBreakup
buildDistanceFareBreakup OneWayFareParameters {..} bookingId = do
  id <- Id <$> generateGUIDText
  let amount = nightShiftRate * fromIntegral distanceFare
      description = "Distance fare is " <> show amount <> " rupees"
  pure FareBreakup {..}

buildDiscountFareBreakup :: MonadGuid m => Maybe Money -> Id Booking -> m (Maybe FareBreakup)
buildDiscountFareBreakup mbDiscount bookingId = do
  forM mbDiscount $ \discount -> do
    id <- Id <$> generateGUIDText
    let amount = fromIntegral $ negate discount -- this amount should be always below zero
        description = "Discount is " <> show discount <> " rupees"
    pure FareBreakup {..}