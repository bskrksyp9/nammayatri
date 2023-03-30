{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Merchant
  ( mapsServiceConfigUpdate,
    mapsServiceUsageConfigUpdate,
    merchantCommonConfigUpdate,
    driverPoolConfigUpdate,
    driverPoolConfigCreate,
    driverIntelligentPoolConfigUpdate,
    onboardingDocumentConfigUpdate,
    onboardingDocumentConfigCreate,
    merchantUpdate,
    smsServiceConfigUpdate,
    smsServiceUsageConfigUpdate,
    verificationServiceConfigUpdate,
  )
where

import Control.Applicative
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as Common
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.DriverPoolConfig as DDPC
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.OnboardingDocumentConfig as DODC
import Environment
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.SMS as SMS
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config as DriverPool
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.DriverIntelligentPoolConfig as CQDIPC
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as CQDPC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CQTC
import qualified Storage.CachedQueries.OnboardingDocumentConfig as CQODC
import Tools.Error

---------------------------------------------------------------------
merchantUpdate :: ShortId DM.Merchant -> Common.MerchantUpdateReq -> Flow Common.MerchantUpdateRes
merchantUpdate merchantShortId req = do
  runRequestValidation Common.validateMerchantUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  let updMerchant =
        merchant{DM.name = fromMaybe merchant.name req.name,
                 DM.description = req.description <|> merchant.description,
                 DM.enabled = fromMaybe merchant.enabled req.enabled
                }
  now <- getCurrentTime

  mbAllExophones <- forM req.exoPhones $ \exophones -> do
    allExophones <- CQExophone.findAllExophones
    let alreadyUsedPhones = getAllPhones $ filter (\exophone -> exophone.merchantId /= merchant.id) allExophones
    let reqPhones = getAllPhones $ toList exophones
    let busyPhones = filter (`elem` alreadyUsedPhones) reqPhones
    unless (null busyPhones) $ do
      throwError $ InvalidRequest $ "Next phones are already in use: " <> show busyPhones
    pure allExophones

  Esq.runTransaction $ do
    CQM.update updMerchant
    whenJust req.exoPhones \exophones -> do
      CQExophone.deleteByMerchantId merchant.id
      forM_ exophones $ \exophoneReq -> do
        exophone <- buildExophone merchant.id now exophoneReq
        CQExophone.create exophone
    whenJust req.fcmConfig $
      \fcmConfig -> CQTC.updateFCMConfig merchant.id fcmConfig.fcmUrl fcmConfig.fcmServiceAccount

  CQM.clearCache updMerchant
  whenJust mbAllExophones $ \allExophones -> do
    let oldExophones = filter (\exophone -> exophone.merchantId == merchant.id) allExophones
    CQExophone.clearCache merchant.id oldExophones
  whenJust req.fcmConfig $ \_ -> CQTC.clearCache merchant.id
  logTagInfo "dashboard -> merchantUpdate : " (show merchant.id)
  return $ mkMerchantUpdateRes updMerchant
  where
    getAllPhones es = (es <&> (.primaryPhone)) <> (es <&> (.backupPhone))

buildExophone :: MonadGuid m => Id DM.Merchant -> UTCTime -> Common.ExophoneReq -> m DExophone.Exophone
buildExophone merchantId now req = do
  uid <- generateGUID
  pure
    DExophone.Exophone
      { id = uid,
        merchantId,
        primaryPhone = req.primaryPhone,
        backupPhone = req.backupPhone,
        isPrimaryDown = False,
        updatedAt = now,
        createdAt = now
      }

mkMerchantUpdateRes :: DM.Merchant -> Common.MerchantUpdateRes
mkMerchantUpdateRes DM.Merchant {..} =
  Common.MerchantUpdateRes
    { name,
      description = description,
      contactNumber = mobileCountryCode <> mobileNumber,
      status = castMerchantStatus status,
      enabled = enabled
    }

castMerchantStatus :: DM.Status -> Common.Status
castMerchantStatus = \case
  DM.PENDING_VERIFICATION -> Common.PENDING_VERIFICATION
  DM.APPROVED -> Common.APPROVED
  DM.REJECTED -> Common.REJECTED

---------------------------------------------------------------------
merchantCommonConfigUpdate :: ShortId DM.Merchant -> Common.MerchantCommonConfigUpdateReq -> Flow APISuccess
merchantCommonConfigUpdate merchantShortId req = do
  runRequestValidation Common.validateMerchantCommonConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  config <- CQTC.findByMerchantId merchant.id >>= fromMaybeM (TransporterConfigNotFound merchant.id.getId)
  let updConfig =
        config{pickupLocThreshold = fromMaybe config.pickupLocThreshold req.pickupLocThreshold,
               dropLocThreshold = fromMaybe config.dropLocThreshold req.dropLocThreshold,
               rideTimeEstimatedThreshold = fromMaybe config.rideTimeEstimatedThreshold req.rideTimeEstimatedThreshold,
               defaultPopupDelay = fromMaybe config.defaultPopupDelay req.defaultPopupDelay,
               popupDelayToAddAsPenalty = req.popupDelayToAddAsPenalty <|> config.popupDelayToAddAsPenalty, -- FIXME we can't update optional field to Nothing
               thresholdCancellationScore = req.thresholdCancellationScore <|> config.thresholdCancellationScore,
               minRidesForCancellationScore = req.minRidesForCancellationScore <|> config.minRidesForCancellationScore,
               waitingTimeEstimatedThreshold = fromMaybe config.waitingTimeEstimatedThreshold req.waitingTimeEstimatedThreshold,
               onboardingTryLimit = fromMaybe config.onboardingTryLimit req.onboardingTryLimit,
               onboardingRetryTimeInHours = fromMaybe config.onboardingRetryTimeInHours req.onboardingRetryTimeInHours,
               checkImageExtractionForDashboard = fromMaybe config.checkImageExtractionForDashboard req.checkImageExtractionForDashboard,
               searchRepeatLimit = fromMaybe config.searchRepeatLimit req.searchRepeatLimit
              }
  Esq.runTransaction $ do
    CQTC.update updConfig
  CQTC.clearCache merchant.id
  logTagInfo "dashboard -> merchantCommonConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
driverPoolConfigUpdate ::
  ShortId DM.Merchant ->
  Meters ->
  Common.DriverPoolConfigUpdateReq ->
  Flow APISuccess
driverPoolConfigUpdate merchantShortId tripDistance req = do
  runRequestValidation Common.validateDriverPoolConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  config <- CQDPC.findByMerchantIdAndTripDistance merchant.id tripDistance >>= fromMaybeM (DriverPoolConfigDoesNotExist merchant.id.getId tripDistance)
  let updConfig =
        config{minRadiusOfSearch = fromMaybe config.minRadiusOfSearch req.minRadiusOfSearch,
               maxRadiusOfSearch = fromMaybe config.maxRadiusOfSearch req.maxRadiusOfSearch,
               radiusStepSize = fromMaybe config.radiusStepSize req.radiusStepSize,
               driverPositionInfoExpiry = req.driverPositionInfoExpiry <|> config.driverPositionInfoExpiry,
               actualDistanceThreshold = req.actualDistanceThreshold <|> config.actualDistanceThreshold,
               maxDriverQuotesRequired = fromMaybe config.maxDriverQuotesRequired req.maxDriverQuotesRequired,
               driverQuoteLimit = fromMaybe config.driverQuoteLimit req.driverQuoteLimit,
               driverRequestCountLimit = fromMaybe config.driverRequestCountLimit req.driverRequestCountLimit,
               driverBatchSize = fromMaybe config.driverBatchSize req.driverBatchSize,
               maxNumberOfBatches = fromMaybe config.maxNumberOfBatches req.maxNumberOfBatches,
               maxParallelSearchRequests = fromMaybe config.maxParallelSearchRequests req.maxParallelSearchRequests,
               poolSortingType = maybe config.poolSortingType castPoolSortingType req.poolSortingType,
               singleBatchProcessTime = fromMaybe config.singleBatchProcessTime req.singleBatchProcessTime
              }
  Esq.runTransaction $ do
    CQDPC.update updConfig
  CQDPC.clearCache merchant.id
  logTagInfo "dashboard -> driverPoolConfigUpdate : " $ show merchant.id <> "tripDistance : " <> show tripDistance
  pure Success

castPoolSortingType :: Common.PoolSortingType -> DriverPool.PoolSortingType
castPoolSortingType = \case
  Common.Intelligent -> DriverPool.Intelligent
  Common.Random -> DriverPool.Random

---------------------------------------------------------------------
driverPoolConfigCreate ::
  ShortId DM.Merchant ->
  Meters ->
  Common.DriverPoolConfigCreateReq ->
  Flow APISuccess
driverPoolConfigCreate merchantShortId tripDistance req = do
  runRequestValidation Common.validateDriverPoolConfigCreateReq req
  merchant <- findMerchantByShortId merchantShortId
  mbConfig <- CQDPC.findByMerchantIdAndTripDistance merchant.id tripDistance
  whenJust mbConfig $ \_ -> throwError (DriverPoolConfigAlreadyExists merchant.id.getId tripDistance)
  newConfig <- buildDriverPoolConfig merchant.id tripDistance req
  Esq.runTransaction $ do
    CQDPC.create newConfig
  -- We should clear cache here, because cache contains list of all configs for current merchantId
  CQDPC.clearCache merchant.id
  logTagInfo "dashboard -> driverPoolConfigCreate : " $ show merchant.id <> "tripDistance : " <> show tripDistance
  pure Success

buildDriverPoolConfig ::
  MonadTime m =>
  Id DM.Merchant ->
  Meters ->
  Common.DriverPoolConfigCreateReq ->
  m DDPC.DriverPoolConfig
buildDriverPoolConfig merchantId tripDistance Common.DriverPoolConfigCreateReq {..} = do
  now <- getCurrentTime
  pure
    DDPC.DriverPoolConfig
      { merchantId,
        poolSortingType = castPoolSortingType poolSortingType,
        updatedAt = now,
        createdAt = now,
        ..
      }

---------------------------------------------------------------------
driverIntelligentPoolConfigUpdate ::
  ShortId DM.Merchant ->
  Common.DriverIntelligentPoolConfigUpdateReq ->
  Flow APISuccess
driverIntelligentPoolConfigUpdate merchantShortId req = do
  runRequestValidation Common.validateDriverIntelligentPoolConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  config <- CQDIPC.findByMerchantId merchant.id >>= fromMaybeM (DriverIntelligentPoolConfigNotFound merchant.id.getId)
  let updConfig =
        config{availabilityTimeWeightage = fromMaybe config.availabilityTimeWeightage req.availabilityTimeWeightage,
               availabilityTimeWindowOption = fromMaybe config.availabilityTimeWindowOption req.availabilityTimeWindowOption,
               acceptanceRatioWeightage = fromMaybe config.acceptanceRatioWeightage req.acceptanceRatioWeightage,
               acceptanceRatioWindowOption = fromMaybe config.acceptanceRatioWindowOption req.acceptanceRatioWindowOption,
               cancellationRatioWeightage = fromMaybe config.cancellationRatioWeightage req.cancellationRatioWeightage,
               cancellationRatioWindowOption = fromMaybe config.cancellationRatioWindowOption req.cancellationRatioWindowOption,
               minQuotesToQualifyForIntelligentPool = fromMaybe config.minQuotesToQualifyForIntelligentPool req.minQuotesToQualifyForIntelligentPool,
               minQuotesToQualifyForIntelligentPoolWindowOption = fromMaybe config.minQuotesToQualifyForIntelligentPoolWindowOption req.minQuotesToQualifyForIntelligentPoolWindowOption,
               intelligentPoolPercentage = req.intelligentPoolPercentage <|> config.intelligentPoolPercentage
              }
  Esq.runTransaction $ do
    CQDIPC.update updConfig
  CQDIPC.clearCache merchant.id
  logTagInfo "dashboard -> driverIntelligentPoolConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
onboardingDocumentConfigUpdate ::
  ShortId DM.Merchant ->
  Common.DocumentType ->
  Common.OnboardingDocumentConfigUpdateReq ->
  Flow APISuccess
onboardingDocumentConfigUpdate merchantShortId reqDocumentType req = do
  runRequestValidation Common.validateOnboardingDocumentConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  let documentType = castDocumentType reqDocumentType
  config <- CQODC.findByMerchantIdAndDocumentType merchant.id documentType >>= fromMaybeM (OnboardingDocumentConfigDoesNotExist merchant.id.getId $ show documentType)
  let updConfig =
        config{checkExtraction = fromMaybe config.checkExtraction req.checkExtraction,
               checkExpiry = fromMaybe config.checkExpiry req.checkExpiry,
               validVehicleClasses = fromMaybe config.validVehicleClasses req.validVehicleClasses,
               vehicleClassCheckType = maybe config.vehicleClassCheckType castVehicleClassCheckType req.vehicleClassCheckType
              }
  Esq.runTransaction $ do
    CQODC.update updConfig
  CQODC.clearCache merchant.id documentType
  logTagInfo "dashboard -> onboardingDocumentConfigUpdate : " $ show merchant.id <> "documentType : " <> show documentType
  pure Success

castVehicleClassCheckType :: Common.VehicleClassCheckType -> DODC.VehicleClassCheckType
castVehicleClassCheckType = \case
  Common.Infix -> DODC.Infix
  Common.Prefix -> DODC.Prefix
  Common.Suffix -> DODC.Suffix

castDocumentType :: Common.DocumentType -> DODC.DocumentType
castDocumentType = \case
  Common.RC -> DODC.RC
  Common.DL -> DODC.DL
  Common.RCInsurance -> DODC.RCInsurance

---------------------------------------------------------------------
onboardingDocumentConfigCreate ::
  ShortId DM.Merchant ->
  Common.DocumentType ->
  Common.OnboardingDocumentConfigCreateReq ->
  Flow APISuccess
onboardingDocumentConfigCreate merchantShortId reqDocumentType req = do
  runRequestValidation Common.validateOnboardingDocumentConfigCreateReq req
  merchant <- findMerchantByShortId merchantShortId
  let documentType = castDocumentType reqDocumentType
  mbConfig <- CQODC.findByMerchantIdAndDocumentType merchant.id documentType
  whenJust mbConfig $ \_ -> throwError (OnboardingDocumentConfigAlreadyExists merchant.id.getId $ show documentType)
  newConfig <- buildOnboardingDocumentConfig merchant.id documentType req
  Esq.runTransaction $ do
    CQODC.create newConfig
  logTagInfo "dashboard -> onboardingDocumentConfigCreate : " $ show merchant.id <> "documentType : " <> show documentType
  pure Success

buildOnboardingDocumentConfig ::
  MonadTime m =>
  Id DM.Merchant ->
  DODC.DocumentType ->
  Common.OnboardingDocumentConfigCreateReq ->
  m DODC.OnboardingDocumentConfig
buildOnboardingDocumentConfig merchantId documentType Common.OnboardingDocumentConfigCreateReq {..} = do
  now <- getCurrentTime
  pure
    DODC.OnboardingDocumentConfig
      { merchantId,
        vehicleClassCheckType = castVehicleClassCheckType vehicleClassCheckType,
        updatedAt = now,
        createdAt = now,
        ..
      }

---------------------------------------------------------------------
mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MapsServiceConfigUpdateReq ->
  Flow APISuccess
mapsServiceConfigUpdate merchantShortId req = do
  merchant <- findMerchantByShortId merchantShortId
  let serviceName = DMSC.MapsService $ Common.getMapsServiceFromReq req
  serviceConfig <- DMSC.MapsServiceConfig <$> Common.buildMapsServiceConfig req
  merchantServiceConfig <- DMSC.buildMerchantServiceConfig merchant.id serviceConfig
  Esq.runTransaction $ do
    CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id serviceName
  logTagInfo "dashboard -> mapsServiceConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
smsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.SmsServiceConfigUpdateReq ->
  Flow APISuccess
smsServiceConfigUpdate merchantShortId req = do
  merchant <- findMerchantByShortId merchantShortId
  let serviceName = DMSC.SmsService $ Common.getSmsServiceFromReq req
  serviceConfig <- DMSC.SmsServiceConfig <$> Common.buildSmsServiceConfig req
  merchantServiceConfig <- DMSC.buildMerchantServiceConfig merchant.id serviceConfig
  Esq.runTransaction $ do
    CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id serviceName
  logTagInfo "dashboard -> smsServiceConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MapsServiceUsageConfigUpdateReq ->
  Flow APISuccess
mapsServiceUsageConfigUpdate merchantShortId req = do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId

  forM_ Maps.availableMapsServices $ \service -> do
    when (Common.mapsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantIdAndService merchant.id (DMSC.MapsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for maps service " <> show service <> " is not provided")

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantId merchant.id
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchant.id.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{getDistances = fromMaybe merchantServiceUsageConfig.getDistances req.getDistances,
                                   getEstimatedPickupDistances = fromMaybe merchantServiceUsageConfig.getEstimatedPickupDistances req.getEstimatedPickupDistances,
                                   getRoutes = fromMaybe merchantServiceUsageConfig.getRoutes req.getRoutes,
                                   snapToRoad = fromMaybe merchantServiceUsageConfig.snapToRoad req.snapToRoad,
                                   getPlaceName = fromMaybe merchantServiceUsageConfig.getPlaceName req.getPlaceName,
                                   getPlaceDetails = fromMaybe merchantServiceUsageConfig.getPlaceDetails req.getPlaceDetails,
                                   autoComplete = fromMaybe merchantServiceUsageConfig.autoComplete req.autoComplete
                                  }
  Esq.runTransaction $ do
    CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchant.id
  logTagInfo "dashboard -> mapsServiceUsageConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
smsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Common.SmsServiceUsageConfigUpdateReq ->
  Flow APISuccess
smsServiceUsageConfigUpdate merchantShortId req = do
  runRequestValidation Common.validateSmsServiceUsageConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId

  forM_ SMS.availableSmsServices $ \service -> do
    when (Common.smsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantIdAndService merchant.id (DMSC.SmsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for sms service " <> show service <> " is not provided")

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantId merchant.id
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchant.id.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{smsProvidersPriorityList = req.smsProvidersPriorityList
                                  }
  Esq.runTransaction $ do
    CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchant.id
  logTagInfo "dashboard -> smsServiceUsageConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
verificationServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.VerificationServiceConfigUpdateReq ->
  Flow APISuccess
verificationServiceConfigUpdate merchantShortId req = do
  merchant <- findMerchantByShortId merchantShortId
  let serviceName = DMSC.VerificationService $ Common.getVerificationServiceFromReq req
  serviceConfig <- DMSC.VerificationServiceConfig <$> Common.buildVerificationServiceConfig req
  merchantServiceConfig <- DMSC.buildMerchantServiceConfig merchant.id serviceConfig
  Esq.runTransaction $ do
    CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id serviceName
  logTagInfo "dashboard -> verificationServiceConfigUpdate : " (show merchant.id)
  pure Success
