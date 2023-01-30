module Domain.Action.Dashboard.Merchant
  ( mapsServiceConfigUpdate,
    mapsServiceUsageConfigUpdate,
    merchantUpdate,
    smsServiceConfigUpdate,
    smsServiceUsageConfigUpdate,
  )
where

import qualified Beckn.External.Maps as Maps
import qualified Beckn.External.SMS as SMS
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Validation
import Control.Applicative ((<|>))
import qualified "dashboard-bpp-helper-api" Dashboard.BAP.Merchant as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import Environment
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import Tools.Error

---------------------------------------------------------------------
merchantUpdate :: ShortId DM.Merchant -> Common.MerchantUpdateReq -> Flow APISuccess
merchantUpdate merchantShortId req = do
  runRequestValidation Common.validateMerchantUpdateReq req
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

  let updMerchant =
        merchant{DM.name = fromMaybe merchant.name req.name,
                 DM.exoPhone = if req.exoPhone == Just "" then Nothing else req.exoPhone <|> merchant.exoPhone,
                 DM.exoPhoneCountryCode = if req.exoPhoneCountryCode == Just "" then Nothing else req.exoPhoneCountryCode <|> merchant.exoPhoneCountryCode,
                 DM.fcmConfig = maybe merchant.fcmConfig (Common.mkFCMConfig merchant.fcmConfig.fcmTokenKeyPrefix) req.fcmConfig,
                 DM.gatewayUrl = fromMaybe merchant.gatewayUrl req.gatewayUrl,
                 DM.registryUrl = fromMaybe merchant.registryUrl req.registryUrl
                }
  Esq.runTransaction $ CQM.update updMerchant
  CQM.clearCache updMerchant
  logTagInfo "dashboard -> merchantUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Common.MapsServiceConfigUpdateReq ->
  Flow APISuccess
mapsServiceConfigUpdate merchantShortId req = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
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
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
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
  whenJust req.getEstimatedPickupDistances $ \_ ->
    throwError (InvalidRequest "getEstimatedPickupDistances is not allowed for bap")
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

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
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

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