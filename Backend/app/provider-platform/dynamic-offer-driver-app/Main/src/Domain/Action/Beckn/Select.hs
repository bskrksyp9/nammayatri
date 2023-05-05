{-# LANGUAGE TypeApplications #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Domain.Action.Beckn.Select where

import qualified Beckn.Types.Core.Taxi.Common.Address as BA
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.Clock (addUTCTime)
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, logDebug, logInfo)
import Lib.Scheduler.JobStorageType.DB.Queries (createJobIn)
import Lib.Scheduler.Types (ExecutionResult (ReSchedule))
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import qualified SharedLogic.CacheDistance as CD
import SharedLogic.DriverPool (getDriverPoolConfig)
import SharedLogic.FareCalculator
import SharedLogic.GoogleMaps
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.FarePolicy as FarePolicyS
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.SearchRequest as QSReq
import Tools.Error
import Tools.Maps as Maps

data DSelectReq = DSelectReq
  { messageId :: Text,
    transactionId :: Text,
    estimateId :: Id DEst.Estimate,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: LatLong,
    pickupTime :: UTCTime,
    dropLocation :: LatLong,
    pickupAddress :: Maybe BA.Address,
    dropAddrress :: Maybe BA.Address,
    autoAssignEnabled :: Bool,
    customerLanguage :: Maybe Maps.Language,
    customerExtraFee :: Maybe Money
  }

type LanguageDictionary = M.Map Maps.Language DSearchReq.SearchRequest

handler :: Id DM.Merchant -> DSelectReq -> Flow ()
handler merchantId sReq = do
  sessiontoken <- generateGUIDText
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  fromLocation <- buildSearchReqLocation merchantId sessiontoken sReq.pickupAddress sReq.customerLanguage sReq.pickupLocation
  toLocation <- buildSearchReqLocation merchantId sessiontoken sReq.dropAddrress sReq.customerLanguage sReq.dropLocation
  mbDistRes <- CD.getCacheDistance sReq.transactionId
  estimate <- QEst.findById sReq.estimateId >>= fromMaybeM (EstimateDoesNotExist sReq.estimateId.getId)
  logInfo $ "Fetching cached distance and duration" <> show mbDistRes
  (distance, duration) <-
    case mbDistRes of
      Nothing -> do
        res <-
          Maps.getDistance merchantId $
            Maps.GetDistanceReq
              { origin = fromLocation,
                destination = toLocation,
                travelMode = Just Maps.CAR
              }
        pure (res.distance, res.duration)
      Just distRes -> pure distRes
  farePolicy <- FarePolicyS.findByMerchantIdAndVariant merchantId estimate.vehicleVariant (Just distance) >>= fromMaybeM NoFarePolicy
  fareParams <-
    calculateFareParameters
      CalculateFareParametersParams
        { farePolicy = farePolicy,
          distance = distance,
          rideTime = sReq.pickupTime,
          waitingTime = Nothing,
          driverSelectedFare = Nothing,
          customerExtraFee = sReq.customerExtraFee
        }
  device <- Redis.get (CD.deviceKey sReq.transactionId)
  searchReq <- buildSearchRequest fromLocation toLocation merchantId estimate sReq distance duration sReq.customerExtraFee device
  let estimateFare = fareSum fareParams
  logDebug $
    "search request id=" <> show searchReq.id
      <> "; estimated distance = "
      <> show distance
      <> "; estimated base fare:"
      <> show estimateFare
  driverPoolConfig <- getDriverPoolConfig merchantId distance
  let inTime = fromIntegral driverPoolConfig.singleBatchProcessTime
  Esq.runTransaction $ do
    QSReq.create searchReq

  res <- sendSearchRequestToDrivers' driverPoolConfig searchReq merchant estimateFare farePolicy.driverExtraFeeBounds
  case res of
    ReSchedule _ -> do
      maxShards <- asks (.maxShards)
      Esq.runTransaction $ do
        createJobIn @_ @'SendSearchRequestToDriver inTime maxShards $
          SendSearchRequestToDriverJobData
            { requestId = searchReq.id,
              baseFare = estimateFare,
              estimatedRideDistance = distance,
              customerExtraFee = sReq.customerExtraFee,
              driverExtraFeeBounds = farePolicy.driverExtraFeeBounds
            }
    _ -> return ()

buildSearchRequest ::
  ( MonadTime m,
    MonadGuid m,
    MonadReader r m,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  Id DM.Merchant ->
  DEst.Estimate ->
  DSelectReq ->
  Meters ->
  Seconds ->
  Maybe Money ->
  Maybe Text ->
  m DSearchReq.SearchRequest
buildSearchRequest from to merchantId estimate sReq distance duration customerExtraFee device = do
  now <- getCurrentTime
  id_ <- Id <$> generateGUID
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill_ = searchRequestExpirationSeconds `addUTCTime` now
  pure
    DSearchReq.SearchRequest
      { id = id_,
        estimateId = estimate.id,
        transactionId = sReq.transactionId,
        messageId = sReq.messageId,
        startTime = sReq.pickupTime,
        validTill = validTill_,
        providerId = merchantId,
        fromLocation = from,
        toLocation = to,
        bapId = sReq.bapId,
        bapUri = sReq.bapUri,
        estimatedDistance = distance,
        estimatedDuration = duration,
        customerExtraFee,
        device = device,
        createdAt = now,
        vehicleVariant = estimate.vehicleVariant,
        status = DSearchReq.ACTIVE,
        autoAssignEnabled = sReq.autoAssignEnabled,
        searchRepeatCounter = 0,
        updatedAt = now
      }

buildSearchReqLocation :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id DM.Merchant -> Text -> Maybe BA.Address -> Maybe Maps.Language -> LatLong -> m DLoc.SearchReqLocation
buildSearchReqLocation merchantId sessionToken address customerLanguage latLong@Maps.LatLong {..} = do
  Address {..} <- case address of
    Just loc
      | customerLanguage == Just Maps.ENGLISH && isJust loc.ward ->
        pure $
          Address
            { areaCode = loc.area_code,
              street = loc.street,
              door = loc.door,
              city = loc.city,
              state = loc.state,
              country = loc.country,
              building = loc.building,
              area = loc.ward,
              full_address = decodeAddress loc
            }
    _ -> getAddressByGetPlaceName merchantId sessionToken latLong
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DLoc.SearchReqLocation {..}

getAddressByGetPlaceName :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id DM.Merchant -> Text -> LatLong -> m Address
getAddressByGetPlaceName merchantId sessionToken latLong = do
  pickupRes <-
    Maps.getPlaceName merchantId $
      Maps.GetPlaceNameReq
        { getBy = Maps.ByLatLong latLong,
          sessionToken = Just sessionToken,
          language = Nothing
        }
  mkLocation pickupRes

decodeAddress :: BA.Address -> Maybe Text
decodeAddress BA.Address {..} = do
  let strictFields = catMaybes $ filter (not . isEmpty) [door, building, street, locality, city, state, area_code, country]
  if null strictFields
    then Nothing
    else Just $ T.intercalate ", " strictFields

isEmpty :: Maybe Text -> Bool
isEmpty = maybe True (T.null . T.replace " " "")
