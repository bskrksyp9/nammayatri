{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FareCalculator
  ( mkBreakupList,
    fareSum,
    CalculateFareParametersParams (..),
    calculateFareParameters,
    isNightShift,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Time
  ( LocalTime (localTimeOfDay),
    TimeOfDay (..),
    TimeZone,
    midnight,
    minutesToTimeZone,
    utcToLocalTime,
  )
import Domain.Types.FareParameters
import qualified Domain.Types.FareParameters as DFParams
import Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy as DFP
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Utils.Common

mkBreakupList :: (Money -> breakupItemPrice) -> (Text -> breakupItemPrice -> breakupItem) -> FareParameters -> [breakupItem]
mkBreakupList mkPrice mkBreakupItem fareParams = do
  let baseFareFinalRounded = fareParams.baseFare
      baseFareCaption = "BASE_FARE"
      baseFareItem = mkBreakupItem baseFareCaption (mkPrice baseFareFinalRounded)

      serviceChargeCaption = "SERVICE_CHARGE"
      mbServiceChargeItem = fmap (mkBreakupItem serviceChargeCaption) (mkPrice <$> fareParams.serviceCharge)

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

      waitingOrPickupChargesCaption = "WAITING_CHARGE"
      mbWaitingOrPickupChargesItem = mkBreakupItem waitingOrPickupChargesCaption . mkPrice <$> fareParams.waitingCharge

      mbFixedGovtRateCaption = "FIXED_GOVERNMENT_RATE"
      mbFixedGovtRateItem = mkBreakupItem mbFixedGovtRateCaption . mkPrice <$> fareParams.govtCharges

      detailsBreakups = processFareParamsDetails fareParams.fareParametersDetails
  catMaybes [Just totalFareItem, Just baseFareItem, mbWaitingOrPickupChargesItem, mbFixedGovtRateItem, mbServiceChargeItem, mbSelectedFareItem, mkCustomerExtraFareItem] <> detailsBreakups
  where
    processFareParamsDetails = \case
      DFParams.ProgressiveDetails det -> mkFPProgressiveDetailsBreakupList det
      DFParams.SlabDetails det -> mkFPSlabDetailsBreakupList det

    mkFPProgressiveDetailsBreakupList det = do
      let deadKmFareCaption = "DEAD_KILOMETER_FARE"
          deadKmFareItem = mkBreakupItem deadKmFareCaption (mkPrice det.deadKmFare)

          extraDistanceFareCaption = "EXTRA_DISTANCE_FARE"
          extraDistanceFareItem =
            det.extraKmFare <&> \extraKmFareRounded ->
              mkBreakupItem extraDistanceFareCaption (mkPrice extraKmFareRounded)
      catMaybes [Just deadKmFareItem, extraDistanceFareItem]

    mkFPSlabDetailsBreakupList _ = []

-- TODO: make some tests for it

fareSum :: FareParameters -> Money
fareSum fareParams = do
  let (partOfNightShiftCharge, notPartOfNightShiftCharge) = countFullFareOfParamsDetails fareParams.fareParametersDetails
  fareParams.baseFare
    + fromMaybe 0 fareParams.driverSelectedFare
    + fromMaybe 0 fareParams.customerExtraFee
    + fromMaybe 0 fareParams.serviceCharge
    + fromMaybe 0 fareParams.waitingCharge
    + partOfNightShiftCharge
    + notPartOfNightShiftCharge

data CalculateFareParametersParams = CalculateFareParametersParams
  { farePolicy :: FarePolicy,
    distance :: Meters,
    rideTime :: UTCTime,
    waitingTime :: Maybe Minutes,
    driverSelectedFare :: Maybe Money,
    customerExtraFee :: Maybe Money
  }

calculateFareParameters ::
  (Monad m, Log m, MonadGuid m, MonadThrow m) =>
  CalculateFareParametersParams ->
  m FareParameters
calculateFareParameters params = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| params.farePolicy.merchantId ||+ " and vehicle variant " +|| params.farePolicy.vehicleVariant ||+ ""
  let fp = params.farePolicy
      mbDriverSelectedFare = params.driverSelectedFare
      mbCustomerExtraFee = params.customerExtraFee
  id <- generateGUID
  let isNightShiftChargeIncluded = isNightShift <$> fp.nightShiftBounds <*> Just params.rideTime
      (baseFare, nightShiftCharge, waitingCharge, fareParametersDetails) = processFarePolicyDetails fp.farePolicyDetails
      (partOfNightShiftCharge, notPartOfNightShiftCharge) = countFullFareOfParamsDetails fareParametersDetails
      fullRideCost {-without govtCharges, waitingCharge, notPartOfNightShiftCharge and nightShift-} =
        baseFare
          + fromMaybe 0 fp.serviceCharge
          + partOfNightShiftCharge
  let resultNightShiftCharge = (\isCoefIncluded -> if isCoefIncluded then countNightShiftCharge fullRideCost <$> nightShiftCharge else Nothing) =<< isNightShiftChargeIncluded
      resultWaitingCharge = countWaitingCharge <$> params.waitingTime <*> waitingCharge
      fullRideCostN {-without govtCharges-} =
        fullRideCost
          + fromMaybe 0 resultNightShiftCharge
          + fromMaybe 0 resultWaitingCharge
          + notPartOfNightShiftCharge
      govtCharges =
        roundToIntegral . (fromIntegral fullRideCostN *) <$> (fp.govtCharges)
      fareParams =
        FareParameters
          { id,
            driverSelectedFare = mbDriverSelectedFare,
            customerExtraFee = mbCustomerExtraFee,
            serviceCharge = fp.serviceCharge,
            waitingCharge = resultWaitingCharge,
            nightShiftCharge = resultNightShiftCharge,
            ..
          }
  logTagInfo "FareCalculator" $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams
  where
    processFarePolicyDetails = \case
      DFP.ProgressiveDetails det -> processFPProgressiveDetails det
      DFP.SlabsDetails det -> processFPSlabsDetailsSlab $ DFP.findFPSlabsDetailsSlabByDistance params.distance det.slabs

    processFPProgressiveDetails DFP.FPProgressiveDetails {..} = do
      let mbExtraDistance =
            params.distance - baseDistance
              & (\dist -> if dist > 0 then Just dist else Nothing)
          mbExtraKmFare = processFPProgressiveDetailsPerExtraKmFare perExtraKmRateSections . metersToKilometers <$> mbExtraDistance
      ( baseFare,
        nightShiftCharge,
        waitingCharge,
        DFParams.ProgressiveDetails $
          DFParams.FParamsProgressiveDetails
            { extraKmFare = mbExtraKmFare,
              ..
            }
        )
    processFPProgressiveDetailsPerExtraKmFare perExtraKmRateSections (extraDistance :: Kilometers) = do
      let sortedPerExtraKmFareSections = NE.sortBy (comparing (.startDistance)) perExtraKmRateSections
      processFPProgressiveDetailsPerExtraKmFare' sortedPerExtraKmFareSections extraDistance
      where
        processFPProgressiveDetailsPerExtraKmFare' _ 0 = 0 :: Money
        processFPProgressiveDetailsPerExtraKmFare' sortedPerExtraKmFareSectionsLeft (extraDistanceLeft :: Kilometers) =
          case sortedPerExtraKmFareSectionsLeft of
            aSection :| [] -> roundToIntegral $ fromIntegral @_ @Centesimal extraDistanceLeft * realToFrac aSection.perExtraKmRate
            aSection :| bSection : leftSections -> do
              let sectionDistance = bSection.startDistance - aSection.startDistance
                  extraDistanceWithinSection = min sectionDistance extraDistanceLeft
              roundToIntegral (fromIntegral @_ @Centesimal extraDistanceWithinSection * realToFrac aSection.perExtraKmRate)
                + processFPProgressiveDetailsPerExtraKmFare' (bSection :| leftSections) (extraDistanceLeft - sectionDistance)

    processFPSlabsDetailsSlab DFP.FPSlabsDetailsSlab {..} = do
      (baseFare, nightShiftCharge, waitingCharge, DFParams.SlabDetails DFParams.FParamsSlabDetails)

    countNightShiftCharge fullRideCost nightShiftCharge = do
      case nightShiftCharge of
        ProgressiveNightShiftCharge charge -> roundToIntegral $ fromIntegral fullRideCost * charge
        ConstantNightShiftCharge charge -> charge

    countWaitingCharge :: Minutes -> WaitingCharge -> Money
    countWaitingCharge waitingTime waitingCharge = do
      case waitingCharge of
        PerMinuteWaitingCharge charge -> roundToIntegral $ fromIntegral waitingTime * charge
        ConstantWaitingCharge charge -> charge

countFullFareOfParamsDetails :: DFParams.FareParametersDetails -> (Money, Money)
countFullFareOfParamsDetails = \case
  DFParams.ProgressiveDetails det -> (fromMaybe 0 det.extraKmFare, det.deadKmFare) -- (partOfNightShiftCharge, notPartOfNightShiftCharge)
  DFParams.SlabDetails _ -> (0, 0)

isNightShift ::
  DFP.NightShiftBounds ->
  UTCTime ->
  Bool
isNightShift nightShiftBounds time = do
  let timeOfDay = localTimeOfDay $ utcToLocalTime timeZoneIST time
  let nightShiftStart = nightShiftBounds.nightShiftStart
  let nightShiftEnd = nightShiftBounds.nightShiftEnd
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
