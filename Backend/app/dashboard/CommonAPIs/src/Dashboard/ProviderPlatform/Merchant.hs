{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Dashboard.ProviderPlatform.Merchant
  ( module Dashboard.ProviderPlatform.Merchant,
    module Reexport,
  )
where

import Dashboard.Common.Merchant as Reexport
import Data.Aeson
import qualified Data.Bifunctor as BF
import Data.ByteString.Lazy as BSL
import Data.Text as T
import Data.Text.Encoding as DT
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Predicate
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Types.Value
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import Servant

---------------------------------------------------------
-- merchant update --------------------------------------

type MerchantUpdateAPI =
  "update"
    :> ReqBody '[JSON] MerchantUpdateReq
    :> Post '[JSON] MerchantUpdateRes

data MerchantUpdateReq = MerchantUpdateReq
  { name :: Maybe Text,
    description :: Maybe Text,
    enabled :: Maybe Bool,
    exoPhones :: Maybe (NonEmpty ExophoneReq),
    fcmConfig :: Maybe FCMConfigUpdateReq
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MerchantUpdateTReq = MerchantUpdateTReq
  { name :: Maybe Text,
    description :: Maybe Text,
    enabled :: Maybe Bool,
    exoPhones :: Maybe (NonEmpty ExophoneReq),
    fcmConfig :: Maybe FCMConfigUpdateTReq
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

validateMerchantUpdateReq :: Validate MerchantUpdateReq
validateMerchantUpdateReq MerchantUpdateReq {..} =
  sequenceA_
    [ validateField "name" name $ InMaybe $ MinLength 3 `And` P.name,
      validateField "description" description $ InMaybe $ MinLength 3 `And` P.name,
      whenJust exoPhones $ \phones -> do
        sequenceA_
          [ validateField "exoPhones" phones $ UniqueField @"primaryPhone",
            validateField "exoPhones" phones $ UniqueField @"backupPhone"
          ],
      whenJust exoPhones $ \phones -> for_ phones $ \exophoneReq -> do
        validateObject "exoPhones" exophoneReq validateExophoneReq,
      whenJust fcmConfig $ \cfg -> validateObject "fcmConfig" cfg validateFCMConfigUpdateReq
    ]

data MerchantUpdateRes = MerchantUpdateRes
  { name :: Text,
    description :: Maybe Text,
    contactNumber :: Maybe Text,
    status :: Status,
    enabled :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Status = PENDING_VERIFICATION | APPROVED | REJECTED
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MerchantUpdateReq where
  type ReqWithoutSecrets MerchantUpdateReq = MerchantUpdateTReq
  hideSecrets MerchantUpdateReq {..} =
    MerchantUpdateTReq
      { fcmConfig = hideSecrets <$> fcmConfig,
        ..
      }

---------------------------------------------------------
-- merchant common config -------------------------------

type MerchantCommonConfigAPI =
  "config"
    :> "common"
    :> Get '[JSON] MerchantCommonConfigRes

data MerchantCommonConfigRes = MerchantCommonConfigRes
  { pickupLocThreshold :: Meters,
    dropLocThreshold :: Meters,
    rideTimeEstimatedThreshold :: Seconds,
    defaultPopupDelay :: Seconds,
    popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int,
    mediaFileUrlPattern :: Text,
    mediaFileSizeUpperLimit :: Int,
    waitingTimeEstimatedThreshold :: Seconds,
    onboardingTryLimit :: Int,
    onboardingRetryTimeInHours :: Int,
    checkImageExtractionForDashboard :: Bool,
    searchRepeatLimit :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- merchant common config update ------------------------

type MerchantCommonConfigUpdateAPI =
  "config"
    :> "common"
    :> "update"
    :> ReqBody '[JSON] MerchantCommonConfigUpdateReq
    :> Post '[JSON] APISuccess

data MerchantCommonConfigUpdateReq = MerchantCommonConfigUpdateReq
  { pickupLocThreshold :: Maybe (MandatoryValue Meters),
    dropLocThreshold :: Maybe (MandatoryValue Meters),
    rideTimeEstimatedThreshold :: Maybe (MandatoryValue Seconds),
    defaultPopupDelay :: Maybe (MandatoryValue Seconds),
    popupDelayToAddAsPenalty :: Maybe (OptionalValue Seconds),
    thresholdCancellationScore :: Maybe (OptionalValue Int),
    minRidesForCancellationScore :: Maybe (OptionalValue Int),
    mediaFileUrlPattern :: Maybe (MandatoryValue Text),
    mediaFileSizeUpperLimit :: Maybe (MandatoryValue Int),
    waitingTimeEstimatedThreshold :: Maybe (MandatoryValue Seconds),
    onboardingTryLimit :: Maybe (MandatoryValue Int),
    onboardingRetryTimeInHours :: Maybe (MandatoryValue Int),
    checkImageExtractionForDashboard :: Maybe (MandatoryValue Bool),
    searchRepeatLimit :: Maybe (MandatoryValue Int)
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MerchantCommonConfigUpdateReq where
  hideSecrets = identity

validateMerchantCommonConfigUpdateReq :: Validate MerchantCommonConfigUpdateReq
validateMerchantCommonConfigUpdateReq MerchantCommonConfigUpdateReq {..} =
  sequenceA_
    [ validateField "pickupLocThreshold" pickupLocThreshold $ InMaybe $ InValue $ Min @Meters 0,
      validateField "dropLocThreshold" dropLocThreshold $ InMaybe $ InValue $ Min @Meters 0,
      validateField "defaultPopupDelay" defaultPopupDelay $ InMaybe $ InValue $ Min @Seconds 0,
      validateField "popupDelayToAddAsPenalty" popupDelayToAddAsPenalty $ InMaybe $ InValue $ Min @Seconds 0,
      validateField "thresholdCancellationScore" thresholdCancellationScore $ InMaybe $ InValue $ InRange @Int 0 100,
      validateField "mediaFileUrlPattern" mediaFileUrlPattern $ InMaybe $ InValue $ MinLength 1,
      validateField "mediaFileSizeUpperLimit" mediaFileSizeUpperLimit $ InMaybe $ InValue $ Min @Int 1,
      validateField "minRidesForCancellationScore" minRidesForCancellationScore $ InMaybe $ InValue $ Min @Int 0,
      validateField "waitingTimeEstimatedThreshold" waitingTimeEstimatedThreshold $ InMaybe $ InValue $ Min @Seconds 0,
      validateField "onboardingTryLimit" onboardingTryLimit $ InMaybe $ InValue $ Min @Int 0,
      validateField "onboardingRetryTimeInHours" onboardingRetryTimeInHours $ InMaybe $ InValue $ Min @Int 0,
      validateField "searchRepeatLimit" searchRepeatLimit $ InMaybe $ InValue $ Min @Int 0
    ]

---------------------------------------------------------
-- merchant driver pool config  -------------------------

type DriverPoolConfigAPI =
  "config"
    :> "driverPool"
    :> QueryParam "tripDistance" Meters
    :> Get '[JSON] DriverPoolConfigRes

type DriverPoolConfigRes = [DriverPoolConfigItem]

data DriverPoolConfigItem = DriverPoolConfigItem
  { minRadiusOfSearch :: Meters,
    maxRadiusOfSearch :: Meters,
    radiusStepSize :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    actualDistanceThreshold :: Maybe Meters,
    maxDriverQuotesRequired :: Int,
    driverQuoteLimit :: Int,
    driverRequestCountLimit :: Int,
    driverBatchSize :: Int,
    maxNumberOfBatches :: Int,
    maxParallelSearchRequests :: Int,
    poolSortingType :: PoolSortingType,
    singleBatchProcessTime :: Seconds,
    tripDistance :: Meters,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PoolSortingType = Intelligent | Random
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- merchant driver pool config update -------------------

type DriverPoolConfigUpdateAPI =
  "config"
    :> "driverPool"
    :> "update"
    :> MandatoryQueryParam "tripDistance" Meters
    :> ReqBody '[JSON] DriverPoolConfigUpdateReq
    :> Post '[JSON] APISuccess

data DriverPoolConfigUpdateReq = DriverPoolConfigUpdateReq
  { minRadiusOfSearch :: Maybe (MandatoryValue Meters),
    maxRadiusOfSearch :: Maybe (MandatoryValue Meters),
    radiusStepSize :: Maybe (MandatoryValue Meters),
    driverPositionInfoExpiry :: Maybe (OptionalValue Seconds),
    actualDistanceThreshold :: Maybe (OptionalValue Meters),
    maxDriverQuotesRequired :: Maybe (MandatoryValue Int),
    driverQuoteLimit :: Maybe (MandatoryValue Int),
    driverRequestCountLimit :: Maybe (MandatoryValue Int),
    driverBatchSize :: Maybe (MandatoryValue Int),
    maxNumberOfBatches :: Maybe (MandatoryValue Int),
    maxParallelSearchRequests :: Maybe (MandatoryValue Int),
    poolSortingType :: Maybe (MandatoryValue PoolSortingType),
    singleBatchProcessTime :: Maybe (MandatoryValue Seconds)
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DriverPoolConfigUpdateReq where
  hideSecrets = identity

validateDriverPoolConfigUpdateReq :: Validate DriverPoolConfigUpdateReq
validateDriverPoolConfigUpdateReq DriverPoolConfigUpdateReq {..} =
  sequenceA_
    [ validateField "minRadiusOfSearch" minRadiusOfSearch $ InMaybe $ InValue $ Min @Meters 1,
      validateField "maxRadiusOfSearch" maxRadiusOfSearch $ InMaybe $ InValue $ Min @Meters (maybe 1 (.value) minRadiusOfSearch),
      validateField "radiusStepSize" radiusStepSize $ InMaybe $ InValue $ Min @Meters 1,
      validateField "driverPositionInfoExpiry" driverPositionInfoExpiry $ InMaybe $ InValue $ Min @Seconds 1,
      validateField "actualDistanceThreshold" actualDistanceThreshold $ InMaybe $ InValue $ Min @Meters 0,
      validateField "maxDriverQuotesRequired" maxDriverQuotesRequired $ InMaybe $ InValue $ Min @Int 1,
      validateField "driverQuoteLimit" driverQuoteLimit $ InMaybe $ InValue $ Min @Int 1,
      validateField "driverRequestCountLimit" driverRequestCountLimit $ InMaybe $ InValue $ Min @Int 1,
      validateField "driverBatchSize" driverBatchSize $ InMaybe $ InValue $ Min @Int 1,
      validateField "maxNumberOfBatches" maxNumberOfBatches $ InMaybe $ InValue $ Min @Int 1,
      validateField "maxParallelSearchRequests" maxParallelSearchRequests $ InMaybe $ InValue $ Min @Int 1,
      validateField "singleBatchProcessTime" singleBatchProcessTime $ InMaybe $ InValue $ Min @Seconds 1
    ]

---------------------------------------------------------
-- merchant driver pool config create -------------------

type DriverPoolConfigCreateAPI =
  "config"
    :> "driverPool"
    :> "create"
    :> MandatoryQueryParam "tripDistance" Meters
    :> ReqBody '[JSON] DriverPoolConfigCreateReq
    :> Post '[JSON] APISuccess

data DriverPoolConfigCreateReq = DriverPoolConfigCreateReq
  { minRadiusOfSearch :: Meters,
    maxRadiusOfSearch :: Meters,
    radiusStepSize :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    actualDistanceThreshold :: Maybe Meters,
    maxDriverQuotesRequired :: Int,
    driverQuoteLimit :: Int,
    driverRequestCountLimit :: Int,
    driverBatchSize :: Int,
    maxNumberOfBatches :: Int,
    maxParallelSearchRequests :: Int,
    poolSortingType :: PoolSortingType,
    singleBatchProcessTime :: Seconds,
    radiusShrinkValueForDriversOnRide :: Int,
    driverToDestinationDistanceThreshold :: Meters,
    driverToDestinationDuration :: Seconds
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DriverPoolConfigCreateReq where
  hideSecrets = identity

validateDriverPoolConfigCreateReq :: Validate DriverPoolConfigCreateReq
validateDriverPoolConfigCreateReq DriverPoolConfigCreateReq {..} =
  sequenceA_
    [ validateField "minRadiusOfSearch" minRadiusOfSearch $ Min @Meters 1,
      validateField "maxRadiusOfSearch" maxRadiusOfSearch $ Min @Meters minRadiusOfSearch,
      validateField "radiusStepSize" radiusStepSize $ Min @Meters 1,
      validateField "driverPositionInfoExpiry" driverPositionInfoExpiry $ InMaybe $ Min @Seconds 1,
      validateField "actualDistanceThreshold" actualDistanceThreshold $ InMaybe $ Min @Meters 0,
      validateField "maxDriverQuotesRequired" maxDriverQuotesRequired $ Min @Int 1,
      validateField "driverQuoteLimit" driverQuoteLimit $ Min @Int 1,
      validateField "driverRequestCountLimit" driverRequestCountLimit $ Min @Int 1,
      validateField "driverBatchSize" driverBatchSize $ Min @Int 1,
      validateField "maxNumberOfBatches" maxNumberOfBatches $ Min @Int 1,
      validateField "maxParallelSearchRequests" maxParallelSearchRequests $ Min @Int 1,
      validateField "singleBatchProcessTime" singleBatchProcessTime $ Min @Seconds 1,
      validateField "radiusShrinkValueForDriversOnRide" radiusShrinkValueForDriversOnRide $ Min @Int 1,
      validateField "driverToDestinationDistanceThreshold" driverToDestinationDistanceThreshold $ Min @Meters 1,
      validateField "driverToDestinationDuration" driverToDestinationDuration $ Min @Seconds 1
    ]

---------------------------------------------------------
-- merchant driver intelligent pool config --------------

type DriverIntelligentPoolConfigAPI =
  "config"
    :> "driverIntelligentPool"
    :> Get '[JSON] DriverIntelligentPoolConfigRes

data DriverIntelligentPoolConfigRes = DriverIntelligentPoolConfigRes
  { availabilityTimeWeightage :: Int,
    availabilityTimeWindowOption :: SWC.SlidingWindowOptions,
    acceptanceRatioWeightage :: Int,
    acceptanceRatioWindowOption :: SWC.SlidingWindowOptions,
    cancellationRatioWeightage :: Int,
    cancellationRatioWindowOption :: SWC.SlidingWindowOptions,
    minQuotesToQualifyForIntelligentPool :: Int,
    minQuotesToQualifyForIntelligentPoolWindowOption :: SWC.SlidingWindowOptions,
    intelligentPoolPercentage :: Maybe Int,
    speedNormalizer :: Double,
    driverSpeedWeightage :: Int,
    minLocationUpdates :: Int,
    locationUpdateSampleTime :: Minutes,
    defaultDriverSpeed :: Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- merchant driver intelligent pool config update -------

type DriverIntelligentPoolConfigUpdateAPI =
  "config"
    :> "driverIntelligentPool"
    :> "update"
    :> ReqBody '[JSON] DriverIntelligentPoolConfigUpdateReq
    :> Post '[JSON] APISuccess

data DriverIntelligentPoolConfigUpdateReq = DriverIntelligentPoolConfigUpdateReq
  { availabilityTimeWeightage :: Maybe (MandatoryValue Int),
    availabilityTimeWindowOption :: Maybe SWC.SlidingWindowOptions, -- value wrapper make no sense for lists and objects
    acceptanceRatioWeightage :: Maybe (MandatoryValue Int),
    acceptanceRatioWindowOption :: Maybe SWC.SlidingWindowOptions,
    cancellationRatioWeightage :: Maybe (MandatoryValue Int),
    cancellationRatioWindowOption :: Maybe SWC.SlidingWindowOptions,
    minQuotesToQualifyForIntelligentPool :: Maybe (MandatoryValue Int),
    minQuotesToQualifyForIntelligentPoolWindowOption :: Maybe SWC.SlidingWindowOptions,
    intelligentPoolPercentage :: Maybe (OptionalValue Int),
    speedNormalizer :: Maybe (MandatoryValue Double),
    driverSpeedWeightage :: Maybe (MandatoryValue Int),
    minLocationUpdates :: Maybe (MandatoryValue Int),
    locationUpdateSampleTime :: Maybe (MandatoryValue Minutes),
    defaultDriverSpeed :: Maybe (MandatoryValue Double)
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DriverIntelligentPoolConfigUpdateReq where
  hideSecrets = identity

validateDriverIntelligentPoolConfigUpdateReq :: Validate DriverIntelligentPoolConfigUpdateReq
validateDriverIntelligentPoolConfigUpdateReq DriverIntelligentPoolConfigUpdateReq {..} =
  sequenceA_
    [ validateField "availabilityTimeWeightage" availabilityTimeWeightage $ InMaybe $ InValue $ InRange @Int (-100) 100,
      whenJust availabilityTimeWindowOption $ \obj ->
        validateObject "availabilityTimeWindowOption" obj validateSlidingWindowOptions,
      validateField "acceptanceRatioWeightage" acceptanceRatioWeightage $ InMaybe $ InValue $ InRange @Int (-100) 100,
      whenJust acceptanceRatioWindowOption $ \obj ->
        validateObject "acceptanceRatioWindowOption" obj validateSlidingWindowOptions,
      validateField "cancellationRatioWeightage" cancellationRatioWeightage $ InMaybe $ InValue $ InRange @Int (-100) 100,
      whenJust cancellationRatioWindowOption $ \obj ->
        validateObject "cancellationRatioWindowOption" obj validateSlidingWindowOptions,
      validateField "minQuotesToQualifyForIntelligentPool" minQuotesToQualifyForIntelligentPool $ InMaybe $ InValue $ Min @Int 1,
      whenJust minQuotesToQualifyForIntelligentPoolWindowOption $ \obj ->
        validateObject "minQuotesToQualifyForIntelligentPoolWindowOption" obj validateSlidingWindowOptions,
      validateField "intelligentPoolPercentage" intelligentPoolPercentage $ InMaybe $ InValue $ InRange @Int 0 100,
      validateField "speedNormalizer" speedNormalizer $ InMaybe $ InValue $ Min @Double 0.0,
      validateField "driverSpeedWeightage" driverSpeedWeightage $ InMaybe $ InValue $ InRange @Int (-100) 100,
      validateField "minLocationUpdates" minLocationUpdates $ InMaybe $ InValue $ Min @Int 0,
      validateField "locationUpdateSampleTime" locationUpdateSampleTime $ InMaybe $ InValue $ Min @Minutes 0,
      validateField "defaultDriverSpeed" defaultDriverSpeed $ InMaybe $ InValue $ Min @Double 0.0
    ]

validateSlidingWindowOptions :: Validate SWC.SlidingWindowOptions
validateSlidingWindowOptions SWC.SlidingWindowOptions {..} =
  validateField "period" period $ Min @Integer 0

---------------------------------------------------------
-- merchant onboarding document config update -----------

type OnboardingDocumentConfigAPI =
  "config"
    :> "onboardingDocument"
    :> QueryParam "documentType" DocumentType
    :> Get '[JSON] OnboardingDocumentConfigRes

type OnboardingDocumentConfigRes = [OnboardingDocumentConfigItem]

data OnboardingDocumentConfigItem = OnboardingDocumentConfigItem
  { documentType :: DocumentType,
    checkExtraction :: Bool,
    checkExpiry :: Bool,
    validVehicleClasses :: [Text],
    vehicleClassCheckType :: VehicleClassCheckType,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleClassCheckType = Infix | Prefix | Suffix
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentType = RC | DL | RCInsurance
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromHttpApiData DocumentType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData DocumentType where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

---------------------------------------------------------
-- merchant onboarding document config update -----------

type OnboardingDocumentConfigUpdateAPI =
  "config"
    :> "onboardingDocument"
    :> "update"
    :> MandatoryQueryParam "documentType" DocumentType
    :> ReqBody '[JSON] OnboardingDocumentConfigUpdateReq
    :> Post '[JSON] APISuccess

data OnboardingDocumentConfigUpdateReq = OnboardingDocumentConfigUpdateReq
  { checkExtraction :: Maybe (MandatoryValue Bool),
    checkExpiry :: Maybe (MandatoryValue Bool),
    validVehicleClasses :: Maybe [Text], -- value wrapper make no sense for lists and objects
    vehicleClassCheckType :: Maybe (MandatoryValue VehicleClassCheckType)
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets OnboardingDocumentConfigUpdateReq where
  hideSecrets = identity

validateOnboardingDocumentConfigUpdateReq :: Validate OnboardingDocumentConfigUpdateReq
validateOnboardingDocumentConfigUpdateReq OnboardingDocumentConfigUpdateReq {..} =
  validateField "validVehicleClasses" validVehicleClasses $ InMaybe $ InList $ MinLength 1

---------------------------------------------------------
-- merchant onboarding document config create -----------

type OnboardingDocumentConfigCreateAPI =
  "config"
    :> "onboardingDocument"
    :> "create"
    :> MandatoryQueryParam "documentType" DocumentType
    :> ReqBody '[JSON] OnboardingDocumentConfigCreateReq
    :> Post '[JSON] APISuccess

data OnboardingDocumentConfigCreateReq = OnboardingDocumentConfigCreateReq
  { checkExtraction :: Bool,
    checkExpiry :: Bool,
    validVehicleClasses :: [Text],
    vehicleClassCheckType :: VehicleClassCheckType
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets OnboardingDocumentConfigCreateReq where
  hideSecrets = identity

validateOnboardingDocumentConfigCreateReq :: Validate OnboardingDocumentConfigCreateReq
validateOnboardingDocumentConfigCreateReq OnboardingDocumentConfigCreateReq {..} =
  validateField "validVehicleClasses" validVehicleClasses $ InList $ MinLength 1
