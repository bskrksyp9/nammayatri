module Environment where

import Beckn.External.Encryption (EncTools)
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config
import qualified Beckn.Storage.Redis.Config as Redis
import qualified Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.Common
import Beckn.Types.Flow
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.Client
import Beckn.Utils.Shutdown
import Tools.Client
import Tools.Metrics

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    redisCfg :: Redis.RedisConfig,
    port :: Int,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    authTokenCacheExpiry :: Seconds,
    registrationTokenExpiry :: Days,
    encTools :: EncTools,
    dataServers :: [DataServer]
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { esqDBEnv :: EsqDBEnv,
    port :: Int,
    loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    graceTerminationPeriod :: Seconds,
    apiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    authTokenCacheExpiry :: Seconds,
    registrationTokenExpiry :: Days,
    encTools :: EncTools,
    coreMetrics :: Metrics.CoreMetricsContainer,
    isShuttingDown :: Shutdown,
    authTokenCacheKeyPrefix :: Text,
    dataServers :: [DataServer]
  }
  deriving (Generic)

buildAppEnv :: Text -> AppCfg -> IO AppEnv
buildAppEnv authTokenCacheKeyPrefix AppCfg {..} = do
  podName <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig podName
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  coreMetrics <- registerCoreMetricsContainer
  isShuttingDown <- mkShutdown
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv