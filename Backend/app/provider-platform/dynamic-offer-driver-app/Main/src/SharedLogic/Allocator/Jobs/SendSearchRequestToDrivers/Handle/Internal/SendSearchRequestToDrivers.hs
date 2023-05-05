{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.SendSearchRequestToDrivers
  ( sendSearchRequestToDrivers,
  )
where

import qualified Data.Map as M
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.Merchant.DriverPoolConfig
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.SearchRequestForDriver
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (addUTCTime, logInfo)
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool (getPoolBatchNum)
import SharedLogic.DriverPool
import SharedLogic.GoogleTranslate
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Tools.Maps as Maps
import qualified Tools.Notifications as Notify

type LanguageDictionary = M.Map Maps.Language DSearchReq.SearchRequest

sendSearchRequestToDrivers ::
  ( Log m,
    EsqDBFlow m r,
    TranslateFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r
  ) =>
  DSR.SearchRequest ->
  Money ->
  Maybe DFP.DriverExtraFeeBounds ->
  DriverPoolConfig ->
  [DriverPoolWithActualDistResult] ->
  m ()
sendSearchRequestToDrivers searchReq baseFare driverExtraFeeBounds driverPoolConfig driverPool = do
  logInfo $ "Send search requests to driver pool batch-" <> show driverPool
  validTill <- getSearchRequestValidTill
  batchNumber <- getPoolBatchNum searchReq.transactionId
  languageDictionary <- foldM (addLanguageToDictionary searchReq) M.empty driverPool
  DS.driverScoreEventHandler
    DST.OnNewSearchRequestForDrivers
      { driverPool = driverPool,
        merchantId = searchReq.providerId,
        searchReq = searchReq,
        validTill = validTill,
        batchProcessTime = fromIntegral driverPoolConfig.singleBatchProcessTime
      }
  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver batchNumber searchReq baseFare validTill) driverPool
  let driverPoolZipSearchRequests = zip driverPool searchRequestsForDrivers
  Esq.runTransaction $ do
    QSRD.setInactiveBySRId searchReq.id -- inactive previous request by drivers so that they can make new offers.
    QSRD.createMany searchRequestsForDrivers
  forM_ driverPoolZipSearchRequests $ \(_, sReqFD) -> do
    Esq.runNoTransaction $ QDFS.updateStatus sReqFD.driverId DDFS.GOT_SEARCH_REQUEST {requestId = sReqFD.searchRequestId, validTill = sReqFD.searchRequestValidTill}

  forM_ driverPoolZipSearchRequests $ \(dPoolRes, sReqFD) -> do
    let language = fromMaybe Maps.ENGLISH dPoolRes.driverPoolResult.language
    let translatedSearchReq = fromMaybe searchReq $ M.lookup language languageDictionary
    let entityData = makeSearchRequestForDriverAPIEntity sReqFD translatedSearchReq dPoolRes.intelligentScores.rideRequestPopupDelayDuration
    Notify.notifyOnNewSearchRequestAvailable searchReq.providerId sReqFD.driverId dPoolRes.driverPoolResult.driverDeviceToken entityData
  where
    getSearchRequestValidTill = do
      now <- getCurrentTime
      let singleBatchProcessTime = fromIntegral driverPoolConfig.singleBatchProcessTime
      return $ singleBatchProcessTime `addUTCTime` now
    buildSearchRequestForDriver ::
      ( MonadFlow m,
        Redis.HedisFlow m r,
        MonadReader r m
      ) =>
      Int ->
      DSearchReq.SearchRequest ->
      Money ->
      UTCTime ->
      DriverPoolWithActualDistResult ->
      m SearchRequestForDriver
    buildSearchRequestForDriver batchNumber searchRequest baseFare_ validTill dpwRes = do
      guid <- generateGUID
      now <- getCurrentTime
      let dpRes = dpwRes.driverPoolResult
      parallelSearchRequestCount <- Just <$> getValidSearchRequestCount searchReq.providerId dpRes.driverId now
      let searchRequestForDriver =
            SearchRequestForDriver
              { id = guid,
                transactionId = searchRequest.transactionId,
                searchRequestId = searchRequest.id,
                startTime = searchRequest.startTime,
                searchRequestValidTill = validTill,
                driverId = cast dpRes.driverId,
                vehicleVariant = dpRes.variant,
                actualDistanceToPickup = dpwRes.actualDistanceToPickup,
                straightLineDistanceToPickup = dpRes.distanceToPickup,
                durationToPickup = dpwRes.actualDurationToPickup,
                status = Active,
                lat = Just dpRes.lat,
                lon = Just dpRes.lon,
                baseFare = baseFare_,
                createdAt = now,
                response = Nothing,
                driverMinExtraFee = driverExtraFeeBounds <&> (.minFee),
                driverMaxExtraFee = driverExtraFeeBounds <&> (.maxFee),
                rideRequestPopupDelayDuration = dpwRes.intelligentScores.rideRequestPopupDelayDuration,
                isPartOfIntelligentPool = dpwRes.isPartOfIntelligentPool,
                acceptanceRatio = dpwRes.intelligentScores.acceptanceRatio,
                cancellationRatio = dpwRes.intelligentScores.cancellationRatio,
                driverAvailableTime = dpwRes.intelligentScores.availableTime,
                driverSpeed = dpwRes.intelligentScores.driverSpeed,
                mode = dpRes.mode,
                ..
              }
      pure searchRequestForDriver

buildTranslatedSearchReqLocation :: (TranslateFlow m r, EsqDBFlow m r, CacheFlow m r) => DLoc.SearchReqLocation -> Maybe Maps.Language -> m DLoc.SearchReqLocation
buildTranslatedSearchReqLocation DLoc.SearchReqLocation {..} mbLanguage = do
  areaRegional <- case mbLanguage of
    Nothing -> return area
    Just lang -> do
      mAreaObj <- translate ENGLISH lang `mapM` area
      let translation = (\areaObj -> listToMaybe areaObj._data.translations) =<< mAreaObj
      return $ (.translatedText) <$> translation
  pure
    DLoc.SearchReqLocation
      { area = areaRegional,
        ..
      }

translateSearchReq ::
  ( TranslateFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DSearchReq.SearchRequest ->
  Maps.Language ->
  m DSearchReq.SearchRequest
translateSearchReq DSearchReq.SearchRequest {..} language = do
  from <- buildTranslatedSearchReqLocation fromLocation (Just language)
  to <- buildTranslatedSearchReqLocation toLocation (Just language)
  pure
    DSearchReq.SearchRequest
      { fromLocation = from,
        toLocation = to,
        ..
      }

addLanguageToDictionary ::
  ( TranslateFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  DSearchReq.SearchRequest ->
  LanguageDictionary ->
  DriverPoolWithActualDistResult ->
  m LanguageDictionary
addLanguageToDictionary searchReq dict dPoolRes = do
  let language = fromMaybe Maps.ENGLISH dPoolRes.driverPoolResult.language
  if isJust $ M.lookup language dict
    then return dict
    else do
      translatedSearchReq <- translateSearchReq searchReq language
      pure $ M.insert language translatedSearchReq dict
