{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Scheduler.Environment where

import Beckn.Mock.App
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import Beckn.Storage.Hedis (HedisCfg, HedisEnv)
import Beckn.Types.Common
import Beckn.Utils.App (Shutdown)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging (LoggerEnv, releaseLoggerEnv)
import qualified Control.Monad.Catch as C
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Map (Map)
import Lib.Scheduler.JobHandler
import Lib.Scheduler.Metrics (SchedulerMetrics)

data SchedulerConfig t = SchedulerConfig
  { loggerConfig :: LoggerConfig,
    metricsPort :: Int,
    esqDBCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    hedisPrefix :: Text,
    port :: Int,
    loopIntervalSec :: Seconds,
    expirationTime :: Integer,
    waitBeforeRetry :: Int,
    jobType :: Maybe t,
    tasksPerIteration :: Int,
    graceTerminationPeriod :: Seconds
  }
  deriving (Generic, FromDhall)

data SchedulerEnv t = SchedulerEnv
  { esqDBEnv :: EsqDBEnv,
    hedisEnv :: HedisEnv,
    loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    metrics :: SchedulerMetrics,
    handlersMap :: Map t (JobHandler t),
    loopIntervalSec :: Seconds,
    expirationTime :: Integer,
    waitBeforeRetry :: Int,
    jobType :: Maybe t,
    tasksPerIteration :: Int,
    graceTerminationPeriod :: Seconds,
    port :: Int,
    isShuttingDown :: Shutdown
  }
  deriving (Generic)

releaseSchedulerEnv :: SchedulerEnv t -> IO ()
releaseSchedulerEnv SchedulerEnv {..} = do
  releaseLoggerEnv loggerEnv

newtype SchedulerM t a = SchedulerM {unSchedulerM :: MockM (SchedulerEnv t) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (SchedulerEnv t), MonadIO)
  deriving newtype (C.MonadThrow, C.MonadCatch, C.MonadMask, MonadClock, MonadTime, MonadGuid, Log, Forkable, MonadUnliftIO)

runSchedulerM :: SchedulerEnv t -> SchedulerM t a -> IO a
runSchedulerM env action = runMock env $ unSchedulerM action