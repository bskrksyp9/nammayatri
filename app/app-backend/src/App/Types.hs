module App.Types
  ( Env,
    Flow,
    FlowHandler,
    FlowServer,
    AppCfg (),
    AppEnv (..),
    buildAppEnv,
  )
where

import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.SesConfig (SesConfig)
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.SignatureAuth
import Data.Time (NominalDiffTime)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Types.Geofencing
import Types.Metrics

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    sesCfg :: SesConfig,
    port :: Int,
    metricsPort :: Int,
    xGatewayUri :: BaseUrl,
    xGatewayApiKey :: Maybe Text,
    xGatewaySelector :: Maybe Text,
    xGatewayNsdlUrl :: Maybe BaseUrl,
    xProviderUri :: BaseUrl,
    bapSelfId :: Text,
    bapNwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    searchConfirmExpiry :: Maybe Integer,
    searchCaseExpiry :: Maybe Integer,
    cronAuthKey :: Maybe CronAuthKey,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    coreVersion :: Text,
    domainVersion :: Text,
    loggerConfig :: LoggerConfig,
    geofencingConfig :: GeofencingConfig,
    signatureExpiry :: NominalDiffTime,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    fcmUrl :: BaseUrl,
    graphhopperUrl :: BaseUrl,
    metricsSearchDurationTimeout :: Int,
    graceTerminationPeriod :: Int
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    sesCfg :: SesConfig,
    xGatewayUri :: BaseUrl,
    xGatewaySelector :: Maybe Text,
    xGatewayNsdlUrl :: Maybe BaseUrl,
    xProviderUri :: BaseUrl,
    bapSelfId :: Text,
    bapNwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    searchConfirmExpiry :: Maybe Integer,
    searchCaseExpiry :: Maybe Integer,
    cronAuthKey :: Maybe CronAuthKey,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    coreVersion :: Text,
    domainVersion :: Text,
    geofencingConfig :: GeofencingConfig,
    signatureExpiry :: NominalDiffTime,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    fcmUrl :: BaseUrl,
    graphhopperUrl :: BaseUrl,
    isShuttingDown :: TMVar (),
    metricsCaseCounter :: CaseCounterMetric,
    metricsSearchDurationTimeout :: Int,
    metricsSearchDuration :: SearchDurationMetric,
    metricsRequestLatency :: RequestLatencyMetric,
    metricsErrorCounter :: ErrorCounterMetric
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  isShuttingDown <- newEmptyTMVarIO
  metricsCaseCounter <- registerCaseCounter
  metricsSearchDuration <- registerSearchDurationMetric metricsSearchDurationTimeout
  metricsRequestLatency <- registerRequestLatencyMetric
  metricsErrorCounter <- registerErrorCounterMetric
  return $
    AppEnv
      { ..
      }

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getSelfId = bapSelfId
  getSelfUrl = bapNwAddress
  getRegistry = credRegistry
  getSigningKeys = signingKeys
  getSignatureExpiry = signatureExpiry
