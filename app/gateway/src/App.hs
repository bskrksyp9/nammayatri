{-# LANGUAGE TypeApplications #-}

module App
  ( runGateway,
  )
where

import App.Server
import App.Types
import Beckn.Constants.APIErrorCode (internalServerErr)
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import qualified Beckn.Types.App as App
import Beckn.Utils.Common (runFlowR)
import Beckn.Utils.Dhall (ZL (Z), readDhallConfigDefault)
import Beckn.Utils.Migration
import qualified Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Cache as C
import EulerHS.Prelude
import EulerHS.Runtime as E
import EulerHS.Types as E
import qualified Network.HTTP.Types as H
import Network.Wai (Response, responseLBS)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setOnExceptionResponse,
    setPort,
  )
import Servant.Server

runGateway :: IO ()
runGateway = do
  appCfg@AppCfg {..} <- readDhallConfigDefault Z "beckn-gateway"
  Metrics.serve metricsPort
  let loggerCfg =
        E.defaultLoggerConfig
          { E._logToFile = True,
            E._logFilePath = "/tmp/beckn-gateway.log",
            E._isAsync = True,
            E._logRawSql = logRawSql
          }
  let settings =
        setOnExceptionResponse gatewayExceptionResponse $
          setPort port defaultSettings
  cache <- C.newCache Nothing
  E.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Initializing Redis Connections..."
    try (runFlowR flowRt appCfg $ prepareRedisConnections redisCfg) >>= \case
      Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
      Right _ -> do
        void $ migrateIfNeeded migrationPath dbCfg autoMigrate
        runSettings settings $ run (App.EnvR flowRt $ mkAppEnv appCfg cache)

gatewayExceptionResponse :: SomeException -> Response
gatewayExceptionResponse exception = do
  let anyException = fromException exception
  case anyException of
    Just ex ->
      responseLBS
        (H.Status (errHTTPCode ex) $ BS.pack $ errReasonPhrase ex)
        ((H.hContentType, "application/json") : errHeaders ex)
        $ errBody ex
    Nothing ->
      responseLBS
        H.internalServerError500
        [(H.hContentType, "application/json")]
        (Aeson.encode internalServerErr)
