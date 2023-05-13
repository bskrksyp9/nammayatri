{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FareCalculator.Calculator
  ( mkBreakupList,
    fareSum,
    baseFareSum,
    calculateFareParameters,
    calculateSlabFareParameters,
    isNightShift,
  )
where

import Data.Time
  ( LocalTime (localTimeOfDay),
    TimeOfDay (..),
    TimeZone,
    midnight,
    minutesToTimeZone,
    utcToLocalTime,
  )
import Domain.Types.FareParameters
import Domain.Types.FarePolicy
import Domain.Types.SlabFarePolicy
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

mkBreakupList :: (Money -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> FareParameters -> [breakupItem]
mkBreakupList mkPrice mkBreakupItem fareParams = do
  case fareParams.farePolicyType of
    SLAB -> mkSlabBreakupList mkPrice mkBreakupItem fareParams
    NORMAL -> mkNormalBreakupList mkPrice mkBreakupItem fareParams

mkSlabBreakupList :: (Money -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> FareParameters -> [breakupItem]
mkSlabBreakupList mkPrice mkBreakupItem fareParams = do
  let baseDistanceFareCaption = "BASE_FARE"
      baseDistanceFareItem = mkBreakupItem baseDistanceFareCaption (mkPrice fareParams.baseFare)

      serviceChargeCaption = "SERVICE_CHARGE"
      mbServiceChargeItem = fmap (mkBreakupItem serviceChargeCaption) (mkPrice <$> fareParams.serviceCharge)

      waitingOrPickupChargesCaption = "WAITING_OR_PICKUP_CHARGES"
      mbWaitingOrPickupChargesItem = fmap (mkBreakupItem waitingOrPickupChargesCaption) (mkPrice <$> fareParams.waitingOrPickupCharges)

      mbFixedGovtRateCaption = "FIXED_GOVERNMENT_RATE"
      mbFixedGovtRateItem = (\fixedGovtRate -> mkBreakupItem mbFixedGovtRateCaption (mkPrice $ fromIntegral ((fixedGovtRate * fromIntegral fareParams.baseFare) `div` 100))) <$> fareParams.govtChargesPerc

      totalFareFinalRounded = fareSum fareParams
      totalFareCaption = "TOTAL_FARE"
      totalFareItem = mkBreakupItem totalFareCaption $ mkPrice totalFareFinalRounded

  [baseDistanceFareItem, totalFareItem] <> catMaybes [mbFixedGovtRateItem, mbServiceChargeItem, mbWaitingOrPickupChargesItem]

mkNormalBreakupList :: (Money -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> FareParameters -> [breakupItem]
mkNormalBreakupList mkPrice mkBreakupItem fareParams = do
  -- TODO: what should be here?
  let dayPartRate = calculateDayPartRate fareParams
      baseFareFinalRounded = roundToIntegral $ fromIntegral fareParams.baseFare * dayPartRate
      baseFareCaption = "BASE_FARE"
      baseFareItem = mkBreakupItem baseFareCaption (mkPrice baseFareFinalRounded)

      deadKmFareCaption = "DEAD_KILOMETER_FARE"
      deadKmFareItem =
        fareParams.deadKmFare <&> \deadKmFare ->
          mkBreakupItem deadKmFareCaption (mkPrice deadKmFare)

      mbExtraKmFareRounded = fareParams.extraKmFare <&> roundToIntegral . (* dayPartRate) . fromIntegral
      extraDistanceFareCaption = "EXTRA_DISTANCE_FARE"
      extraDistanceFareItem =
        mbExtraKmFareRounded <&> \extraKmFareRounded ->
          mkBreakupItem extraDistanceFareCaption (mkPrice extraKmFareRounded)

      mkSelectedFareCaption = "DRIVER_SELECTED_FARE"
      mbSelectedFareItem =
        fareParams.driverSelectedFare <&> \selFare ->
          mkBreakupItem mkSelectedFareCaption (mkPrice selFare)

      customerExtraFareCaption = "CUSTOMER_SELECTED_FARE"
      mkCustomerExtraFareItem =
        fareParams.customerExtraFee <&> \ceFare -> do
          mkBreakupItem customerExtraFareCaption (mkPrice ceFare)

      totalFareFinalRounded = fareSum fareParams
      totalFareCaption = "TOTAL_FARE"
      totalFareItem = mkBreakupItem totalFareCaption $ mkPrice totalFareFinalRounded
  catMaybes [Just totalFareItem, Just baseFareItem, deadKmFareItem, extraDistanceFareItem, mbSelectedFareItem, mkCustomerExtraFareItem]

-- TODO: make some tests for it

fareSum :: FareParameters -> Money
fareSum fareParams = case fareParams.farePolicyType of
  NORMAL -> normalFareSum fareParams
  SLAB -> slabFareSum fareParams

normalFareSum :: FareParameters -> Money
normalFareSum fareParams = do
  baseFareSum fareParams + (fromMaybe 0 fareParams.deadKmFare) + fromMaybe 0 fareParams.driverSelectedFare

slabFareSum :: FareParameters -> Money
slabFareSum = baseFareSum

addGovtCharges :: FareParameters -> Money
addGovtCharges fp =
  maybe 0 (\govtChargesPerc -> (fp.baseFare * fromIntegral govtChargesPerc) `div` 100) fp.govtChargesPerc

baseFareSum :: FareParameters -> Money
baseFareSum fareParams =
  case fareParams.farePolicyType of
    SLAB -> baseSlabFareSum
    NORMAL -> baseNormalFareSum
  where
    baseSlabFareSum = do
      let fareAmount = addGovtCharges fareParams + (fromMaybe 0 fareParams.serviceCharge) + fromMaybe 0 fareParams.waitingOrPickupCharges + fromMaybe 0 fareParams.driverSelectedFare
      (fareAmount +) . roundToIntegral $
        if fareParams.nightCoefIncluded
          then fromIntegral fareParams.baseFare + (fromMaybe 0 fareParams.nightShiftRate) -- using rate as value here
          else fromIntegral fareParams.baseFare

    baseNormalFareSum = roundToIntegral $ do
      let dayPartCoef = calculateDayPartRate fareParams
      dayPartCoef * (fromIntegral fareParams.baseFare + maybe 0 fromIntegral fareParams.extraKmFare)
        + maybe 0 fromIntegral fareParams.customerExtraFee

calculateDayPartRate :: FareParameters -> Centesimal
calculateDayPartRate fareParams = do
  let defaultDayPartRate = 1
  if fareParams.nightCoefIncluded
    then fromMaybe defaultDayPartRate fareParams.nightShiftRate
    else defaultDayPartRate

calculateFareParameters ::
  MonadGuid m =>
  FarePolicy ->
  Meters ->
  UTCTime ->
  Maybe Money ->
  Maybe Money ->
  m FareParameters
calculateFareParameters fp distance time mbExtraFare mbCustomerExtraFee = do
  let baseDistanceFare = roundToIntegral $ fp.baseDistanceFare
      mbExtraDistance =
        distance - fp.baseDistanceMeters
          & (\dist -> if dist > 0 then Just dist else Nothing)
      mbExtraKmFare = mbExtraDistance <&> \ex -> roundToIntegral $ realToFrac (distanceToKm ex) * fp.perExtraKmFare
      nightCoefIncluded = isNightShift fp.nightShiftStart fp.nightShiftEnd time

  id <- generateGUID
  pure
    FareParameters
      { id,
        baseFare = baseDistanceFare,
        deadKmFare = Just fp.deadKmFare,
        extraKmFare = mbExtraKmFare,
        driverSelectedFare = mbExtraFare,
        customerExtraFee = mbCustomerExtraFee,
        nightShiftRate = fp.nightShiftRate,
        nightCoefIncluded,
        waitingChargePerMin = fp.waitingChargePerMin,
        waitingOrPickupCharges = Nothing,
        serviceCharge = Nothing,
        farePolicyType = NORMAL,
        govtChargesPerc = Nothing
      }

calculateSlabFareParameters ::
  (MonadGuid m, MonadThrow m, Log m) =>
  SlabFarePolicy ->
  Meters ->
  UTCTime ->
  Maybe Money ->
  Maybe Money ->
  m FareParameters
calculateSlabFareParameters fp distance time mbExtraFare customerExtraFee = do
  let mbSlab = find (selectSlab distance) fp.fareSlabs
  when (isNothing mbSlab) $ throwError (InvalidRequest "Fare slab not found")
  let slab = fromJust mbSlab

  let baseDistanceFare = roundToIntegral $ slab.fare
      waitingOrPickupCharges = roundToIntegral $ slab.waitingCharge
      nightCoefIncluded = isNightShift fp.nightShiftStart fp.nightShiftEnd time
  id <- generateGUID
  pure
    FareParameters
      { id,
        baseFare = baseDistanceFare,
        customerExtraFee = customerExtraFee,
        deadKmFare = Nothing,
        extraKmFare = Nothing,
        serviceCharge = Just fp.serviceCharge,
        driverSelectedFare = mbExtraFare,
        nightShiftRate = Just $ fromIntegral (roundToIntegral $ slab.nightCharge :: Money), -- todo: make the name better
        nightCoefIncluded,
        waitingOrPickupCharges = Just waitingOrPickupCharges,
        waitingChargePerMin = Nothing,
        farePolicyType = SLAB,
        govtChargesPerc = fp.govtChargesPerc
      }

selectSlab :: Meters -> Slab -> Bool
selectSlab distance slab = distance >= slab.startMeters && distance <= slab.endMeters

distanceToKm :: Meters -> Rational
distanceToKm x = realToFrac x / 1000

isNightShift ::
  Maybe TimeOfDay ->
  Maybe TimeOfDay ->
  UTCTime ->
  Bool
isNightShift mbNightShiftStart mbNightShiftEnd time = do
  let timeOfDay = localTimeOfDay $ utcToLocalTime timeZoneIST time
  let nightShiftStart = fromMaybe midnight mbNightShiftStart
  let nightShiftEnd = fromMaybe midnight mbNightShiftEnd
  isTimeWithinBounds nightShiftStart nightShiftEnd timeOfDay

timeZoneIST :: TimeZone
timeZoneIST = minutesToTimeZone 330 -- TODO: Should be configurable. Hardcoded to IST +0530

isTimeWithinBounds :: TimeOfDay -> TimeOfDay -> TimeOfDay -> Bool
isTimeWithinBounds startTime endTime time =
  if startTime >= endTime
    then do
      let midnightBeforeTimeleap = TimeOfDay 23 59 60
      (startTime < time && time < midnightBeforeTimeleap) || (midnight <= time && time < endTime)
    else startTime < time && time < endTime
