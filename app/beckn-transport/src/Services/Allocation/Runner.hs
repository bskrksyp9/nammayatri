module Services.Allocation.Runner where

import App.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Beckn.Utils.Common
import qualified Beckn.Utils.Logging as Log
import Control.Concurrent.STM.TMVar (isEmptyTMVar)
import Data.Time (diffUTCTime, nominalDiffTimeToSeconds)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Services.Allocation.Allocation as Allocation
import qualified Services.Allocation.Internal as I

handle :: Allocation.ServiceHandle Flow
handle =
  Allocation.ServiceHandle
    { getDriverSortMode = I.getDriverSortMode,
      getConfiguredNotificationTime = I.getConfiguredNotificationTime,
      getConfiguredAllocationTime = I.getConfiguredAllocationTime,
      getRequests = I.getRequests,
      getDriverPool = I.getDriverPool,
      getCurrentNotification = I.getCurrentNotification,
      sendNewRideNotification = I.sendNewRideNotification,
      sendRideNotAssignedNotification = I.sendRideNotAssignedNotification,
      addNotificationStatus = I.addNotificationStatus,
      updateNotificationStatus = I.updateNotificationStatus,
      resetLastRejectionTime = I.resetLastRejectionTime,
      getAttemptedDrivers = I.getAttemptedDrivers,
      getDriversWithNotification = I.getDriversWithNotification,
      getFirstDriverInTheQueue = I.getFirstDriverInTheQueue,
      checkAvailability = I.checkAvailability,
      getDriverResponse = I.getDriverResponse,
      assignDriver = I.assignDriver,
      cancelRide = I.cancelRide,
      addAllocationRequest = I.addAllocationRequest,
      getRideInfo = I.getRideInfo,
      cleanupNotifications = I.cleanupNotifications,
      removeRequest = I.removeRequest,
      logEvent = I.logEvent
    }

run :: TMVar () -> TMVar () -> Flow ()
run shutdown activeTask = do
  Redis.tryLockRedis "allocation" 10 >>= \case
    False -> L.runIO $ threadDelay 5000000 -- sleep for a bit
    _ -> do
      now <- getCurrentTime
      Redis.setKeyRedis "beckn:allocation:service" now
      L.runIO $ atomically $ putTMVar activeTask ()
      processStartTime <- getCurrentTime
      requestsNum <- asks (requestsNumPerIteration . driverAllocationConfig)
      eres <- runSafeFlow $ Allocation.process handle requestsNum
      whenLeft eres $ Log.logError "Allocation service"
      Redis.unlockRedis "allocation"
      processEndTime <- getCurrentTime
      let processTime = diffUTCTime processEndTime processStartTime
      L.runIO $ atomically $ takeTMVar activeTask
      -- If process handling took less than processDelay we delay for remain to processDelay time
      processDelay <- asks (processDelay . driverAllocationConfig)
      L.runIO $ threadDelay $ fromNominalToMicroseconds $ max 0 (processDelay - processTime)
  isRunning <- L.runIO $ liftIO $ atomically $ isEmptyTMVar shutdown
  when isRunning $ run shutdown activeTask
  where
    fromNominalToMicroseconds = floor . (1000000 *) . nominalDiffTimeToSeconds
