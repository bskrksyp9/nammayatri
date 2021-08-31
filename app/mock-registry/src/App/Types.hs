module App.Types
  ( Env,
    FlowHandler,
    FlowServer,
    AppCfg (),
    AppEnv (..),
    buildAppEnv,
  )
where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Credentials
import Beckn.Types.Flow
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetricsContainer, registerCoreMetricsContainer)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import Utils.Auth

data AppCfg = AppCfg
  { port :: Int,
    serverUrl :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    signatureExpiry :: Seconds,
    graceTerminationPeriod :: Int
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { serverUrl :: BaseUrl,
    credRegistry :: [Credential],
    signingKeys :: [SigningKey],
    signatureExpiry :: Seconds,
    isShuttingDown :: TMVar (),
    coreMetrics :: CoreMetricsContainer
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  isShuttingDown <- newEmptyTMVarIO
  coreMetrics <- registerCoreMetricsContainer
  return $
    AppEnv
      { ..
      }

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

instance AuthenticatingEntity AppEnv where
  getSelfUrl = serverUrl
  getRegistry = credRegistry
  getSigningKeys = signingKeys
  getSignatureExpiry = signatureExpiry

instance HasLookupAction LookupRegistryOrg (FlowR AppEnv) where
  runLookup = lookup
