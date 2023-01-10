module Environment where

import Beckn.Mock.ExternalAPI
import Beckn.Storage.Hedis
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Servant.SignatureAuth hiding (prepareAuthManager)
import Control.Monad.Catch (bracket)
import Network.HTTP.Client (Manager, newManager)
import Relude

data AppCfg = AppCfg
  { port :: Int,
    selfId :: Text,
    uniqueKeyId :: Text,
    selfUri :: BaseUrl,
    hedisCfg :: HedisCfg,
    statusWaitTimeSec :: Seconds,
    callbackWaitTimeMilliSec :: Milliseconds,
    loggerConfig :: LoggerConfig,
    authEntity :: AuthenticatingEntity'
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { selfId :: Text,
    uniqueKeyId :: Text,
    selfUri :: BaseUrl,
    statusWaitTimeSec :: Seconds,
    callbackWaitTimeMilliSec :: Milliseconds,
    loggerConfig :: LoggerConfig,
    authEntity :: AuthenticatingEntity',
    hedisEnv :: HedisEnv,
    loggerEnv :: LoggerEnv,
    authManager :: Manager
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv config@AppCfg {..} = do
  hedisEnv <- connectHedis hedisCfg ("mock_public_transport_bpp" <>)
  loggerEnv <- prepareLoggerEnv loggerConfig Nothing
  let authManagerSettings = prepareAuthManager config ["Authorization"] selfId uniqueKeyId (logOutputIO loggerEnv)
  authManager <- newManager authManagerSettings
  return $ AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  disconnectHedis hedisEnv
  releaseLoggerEnv loggerEnv

withAppEnv :: AppCfg -> (AppEnv -> IO ()) -> IO ()
withAppEnv cfg = bracket (buildAppEnv cfg) releaseAppEnv

instance AuthenticatingEntity AppCfg where
  getSigningKey = (.authEntity.signingKey)
  getSignatureExpiry = (.authEntity.signatureExpiry)