module App.Types
  ( AppCfg (),
    AppEnv (..),
    Env,
    FlowHandler,
    FlowServer,
    Log (..),
    buildAppEnv,
  )
where

import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Types.Flow
import Beckn.Types.SlidingWindowLimiter
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.Client (HttpClientOptions)
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Types.Metrics
import Utils.Auth

data AppCfg = AppCfg
  { dbCfg :: DBConfig,
    redisCfg :: T.RedisConfig,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    inviteSmsTemplate :: Text,
    port :: Int,
    bgtmPort :: Int,
    metricsPort :: Int,
    xGatewaySelector :: Maybe Text,
    xGatewayNsdlUrl :: Maybe BaseUrl,
    xAppUri :: BaseUrl,
    selfId :: Text,
    nwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    caseExpiry :: Maybe Seconds,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool,
    coreVersion :: Text,
    domainVersion :: Text,
    loggerConfig :: LoggerConfig,
    signatureExpiry :: Seconds,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    fcmUrl :: BaseUrl,
    graphhopperUrl :: BaseUrl,
    graceTerminationPeriod :: Seconds,
    defaultRadiusOfSearch :: Meters,
    apiRateLimitOptions :: APIRateLimitOptions,
    httpClientOptions :: HttpClientOptions,
    authTokenCacheExpiry :: Seconds,
    minimumDriverRatesCount :: Int,
    recalculateFareEnabled :: Bool,
    updateLocationRefreshPeriod :: Seconds
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    smsCfg :: SmsConfig,
    otpSmsTemplate :: Text,
    inviteSmsTemplate :: Text,
    xGatewaySelector :: Maybe Text,
    xGatewayNsdlUrl :: Maybe BaseUrl,
    xAppUri :: BaseUrl,
    selfId :: Text,
    nwAddress :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    caseExpiry :: Maybe Seconds,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    coreVersion :: Text,
    domainVersion :: Text,
    signatureExpiry :: Seconds,
    googleMapsUrl :: BaseUrl,
    googleMapsKey :: Text,
    fcmUrl :: BaseUrl,
    graphhopperUrl :: BaseUrl,
    isShuttingDown :: TMVar (),
    coreMetrics :: CoreMetricsContainer,
    transporterMetrics :: TransporterMetricsContainer,
    apiRateLimitOptions :: APIRateLimitOptions,
    defaultRadiusOfSearch :: Meters,
    httpClientOptions :: HttpClientOptions,
    authTokenCacheExpiry :: Seconds,
    minimumDriverRatesCount :: Int,
    recalculateFareEnabled :: Bool,
    updateLocationRefreshPeriod :: Seconds
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  coreMetrics <- registerCoreMetricsContainer
  transporterMetrics <- registerTransporterMetricsContainer
  isShuttingDown <- newEmptyTMVarIO
  return AppEnv {..}

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getSelfUrl = nwAddress
  getRegistry = credRegistry
  getSigningKeys = signingKeys
  getSignatureExpiry = signatureExpiry

instance HasLookupAction LookupRegistryOrg (FlowR AppEnv) where
  runLookup = lookup
