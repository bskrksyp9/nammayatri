{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle
  ( HandleMonad,
    Handle (..),
    MetricsHandle (..),
    handler,
  )
where

import Kernel.Prelude
import Kernel.Utils.Common
import Lib.Scheduler.Types (ExecutionResult (..))
import SharedLogic.DriverPool

type HandleMonad m = (Monad m, MonadClock m, Log m)

data MetricsHandle m = MetricsHandle
  { incrementTaskCounter :: m (),
    incrementFailedTaskCounter :: m (),
    putTaskDuration :: Milliseconds -> m ()
  }

data Handle m = Handle
  { isBatchNumExceedLimit :: m Bool,
    isRideAlreadyAssigned :: m Bool,
    isReceivedMaxDriverQuotes :: m Bool,
    getNextDriverPoolBatch :: m [DriverPoolWithActualDistResult],
    sendSearchRequestToDrivers :: [DriverPoolWithActualDistResult] -> m (),
    getRescheduleTime :: m UTCTime,
    metrics :: MetricsHandle m,
    setBatchDurationLock :: m (Maybe UTCTime),
    createRescheduleTime :: UTCTime -> m UTCTime,
    ifSearchRequestIsInvalid :: m Bool
  }

handler :: HandleMonad m => Handle m -> m ExecutionResult
handler h@Handle {..} = do
  logInfo "Starting job execution"
  metrics.incrementTaskCounter
  measuringDuration (\ms _ -> metrics.putTaskDuration ms) $ do
    isSearchRequestInvalid <- ifSearchRequestIsInvalid
    if isSearchRequestInvalid
      then do
        logInfo "Search request is either cancelled or expired."
        return Complete
      else do
        isRideAssigned <- isRideAlreadyAssigned
        if isRideAssigned
          then do
            logInfo "Ride already assigned."
            return Complete
          else do
            isReceivedMaxDriverQuotes' <- isReceivedMaxDriverQuotes
            if isReceivedMaxDriverQuotes'
              then do
                logInfo "Received enough quotes from drivers."
                return Complete
              else processRequestSending h

processRequestSending :: HandleMonad m => Handle m -> m ExecutionResult
processRequestSending Handle {..} = do
  mLastProcTime <- setBatchDurationLock
  case mLastProcTime of
    Just lastProcTime -> ReSchedule <$> createRescheduleTime lastProcTime
    Nothing -> do
      isBatchNumExceedLimit' <- isBatchNumExceedLimit
      if isBatchNumExceedLimit'
        then do
          metrics.incrementFailedTaskCounter
          logInfo "No driver accepted"
          return Complete
        else do
          driverPool <- getNextDriverPoolBatch
          if null driverPool
            then do
              metrics.incrementFailedTaskCounter
              logInfo "No driver available"
              return Complete
            else do
              sendSearchRequestToDrivers driverPool
              ReSchedule <$> getRescheduleTime
