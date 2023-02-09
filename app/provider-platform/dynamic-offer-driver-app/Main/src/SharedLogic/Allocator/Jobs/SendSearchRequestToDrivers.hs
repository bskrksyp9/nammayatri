module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers where

import Domain.Types.Merchant (Merchant)
import Domain.Types.SearchRequest (SearchRequest)
import Kernel.Prelude hiding (handle)
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config (HasSendSearchRequestJobConfig)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal as I
import SharedLogic.DriverPool.Config (DriverPoolConfig, HasDriverPoolConfig, getDriverPoolConfig)
import qualified SharedLogic.DriverPool.Config as DP
import SharedLogic.GoogleTranslate (TranslateFlow)
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.SearchRequest as QSR
import qualified Tools.Metrics as Metrics

sendSearchRequestToDrivers ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    Metrics.CoreMetrics m,
    HasSendSearchRequestJobConfig r,
    HasDriverPoolConfig r,
    HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  Job 'SendSearchRequestToDriver ->
  m ExecutionResult
sendSearchRequestToDrivers Job {id, jobData} = withLogTag ("JobId-" <> id.getId) do
  let searchReqId = jobData.requestId
  searchReq <- QSR.findById searchReqId >>= fromMaybeM (SearchRequestNotFound searchReqId.getId)
  merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound (searchReq.providerId.getId))
  driverPoolConfig <- getDriverPoolConfig jobData.estimatedRideDistance
  sendSearchRequestToDrivers' driverPoolConfig searchReq merchant jobData.baseFare jobData.driverMinExtraFee jobData.driverMaxExtraFee

sendSearchRequestToDrivers' ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    Metrics.CoreMetrics m,
    HasSendSearchRequestJobConfig r,
    DP.HasDriverPoolConfig r,
    HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  DriverPoolConfig ->
  SearchRequest ->
  Merchant ->
  Money ->
  Money ->
  Money ->
  m ExecutionResult
sendSearchRequestToDrivers' driverPoolConfig searchReq merchant baseFare driverMinExtraCharge driverMaxExtraCharge = do
  handler handle
  where
    handle =
      Handle
        { isBatchNumExceedLimit = I.isBatchNumExceedLimit searchReq.id,
          isRideAlreadyAssigned = I.isRideAlreadyAssigned searchReq.id,
          isReceivedMaxDriverQuotes = I.isReceivedMaxDriverQuotes driverPoolConfig searchReq.id,
          getNextDriverPoolBatch = I.getNextDriverPoolBatch driverPoolConfig searchReq,
          cleanupDriverPoolBatches = I.cleanupDriverPoolBatches searchReq.id,
          sendSearchRequestToDrivers = I.sendSearchRequestToDrivers searchReq baseFare driverMinExtraCharge driverMaxExtraCharge,
          getRescheduleTime = I.getRescheduleTime,
          setBatchDurationLock = I.setBatchDurationLock searchReq.id,
          createRescheduleTime = I.createRescheduleTime,
          metrics =
            MetricsHandle
              { incrementTaskCounter = Metrics.incrementTaskCounter merchant.name,
                incrementFailedTaskCounter = Metrics.incrementFailedTaskCounter merchant.name,
                putTaskDuration = Metrics.putTaskDuration merchant.name
              },
          ifSearchRequestIsCancelled = I.ifSearchRequestIsCancelled searchReq.id
        }