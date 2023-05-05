{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers where

import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.SearchRequest (SearchRequest)
import Kernel.Prelude hiding (handle)
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow)
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal as I
import SharedLogic.DriverPool
import SharedLogic.GoogleTranslate (TranslateFlow)
import Storage.CachedQueries.CacheConfig (CacheFlow, HasCacheConfig)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.SearchRequest as QSR
import qualified Tools.Metrics as Metrics

sendSearchRequestToDrivers ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    Metrics.CoreMetrics m,
    HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  Job 'SendSearchRequestToDriver ->
  m ExecutionResult
sendSearchRequestToDrivers Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let searchReqId = jobData.requestId
  searchReq <- QSR.findById searchReqId >>= fromMaybeM (SearchRequestNotFound searchReqId.getId)
  merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound (searchReq.providerId.getId))
  driverPoolConfig <- getDriverPoolConfig merchant.id jobData.estimatedRideDistance
  sendSearchRequestToDrivers' driverPoolConfig searchReq merchant jobData.baseFare jobData.driverExtraFeeBounds

sendSearchRequestToDrivers' ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    Metrics.CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    Log m
  ) =>
  DriverPoolConfig ->
  SearchRequest ->
  Merchant ->
  Money ->
  Maybe DFP.DriverExtraFeeBounds ->
  m ExecutionResult
sendSearchRequestToDrivers' driverPoolConfig searchReq merchant baseFare driverExtraFeeBounds = do
  handler handle
  where
    handle =
      Handle
        { isBatchNumExceedLimit = I.isBatchNumExceedLimit driverPoolConfig searchReq.transactionId,
          isRideAlreadyAssigned = I.isRideAlreadyAssigned searchReq.id,
          isReceivedMaxDriverQuotes = I.isReceivedMaxDriverQuotes driverPoolConfig searchReq.id,
          getNextDriverPoolBatch = I.getNextDriverPoolBatch driverPoolConfig searchReq,
          sendSearchRequestToDrivers = I.sendSearchRequestToDrivers searchReq baseFare driverExtraFeeBounds driverPoolConfig,
          getRescheduleTime = I.getRescheduleTime driverPoolConfig.singleBatchProcessTime,
          setBatchDurationLock = I.setBatchDurationLock searchReq.id driverPoolConfig.singleBatchProcessTime,
          createRescheduleTime = I.createRescheduleTime driverPoolConfig.singleBatchProcessTime,
          metrics =
            MetricsHandle
              { incrementTaskCounter = Metrics.incrementTaskCounter merchant.name,
                incrementFailedTaskCounter = Metrics.incrementFailedTaskCounter merchant.name,
                putTaskDuration = Metrics.putTaskDuration merchant.name
              },
          ifSearchRequestIsInvalid = I.ifSearchRequestInvalid searchReq.id
        }
