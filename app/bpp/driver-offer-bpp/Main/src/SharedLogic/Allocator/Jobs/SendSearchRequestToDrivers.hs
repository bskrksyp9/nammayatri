module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers where

import Beckn.Prelude hiding (handle)
import Beckn.Storage.Esqueleto (EsqDBReplicaFlow)
import Beckn.Storage.Hedis (HedisFlow)
import Beckn.Types.Error
import qualified Beckn.Types.SlidingWindowCounters as SWC
import Beckn.Utils.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.SearchRequest (SearchRequest)
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config (HasSendSearchRequestJobConfig)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal as I
import SharedLogic.DriverPool.Config (DriverPoolConfig, HasDriverPoolConfig, getDriverPoolConfig)
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
    Log m,
    SWC.HasWindowOptions r
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
    HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m,
    SWC.HasWindowOptions r
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
              }
        }