{-# LANGUAGE TypeApplications #-}

module App where

import qualified App.Server as App
import App.Types
import Beckn.Constants.APIErrorCode
import Beckn.Storage.DB.Config
import Beckn.Types.App
import qualified Beckn.Types.App as App
import Beckn.Utils.Common (prepareAppOptions, runFlowR)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as T
import qualified Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Handler.Warp
  ( Settings,
    defaultSettings,
    runSettings,
    setOnExceptionResponse,
    setPort,
  )
import Servant
import Storage.DB.Config as Config
import Storage.Redis.Config
import qualified System.Environment as SE

runTransporterBackendApp :: IO ()
runTransporterBackendApp = do
  port <- fromMaybe 8014 . (>>= readMaybe) <$> SE.lookupEnv "PORT"
  runTransporterBackendApp' port $
    setOnExceptionResponse transporterExceptionResponse $
      setPort port defaultSettings

runTransporterBackendApp' :: Int -> Settings -> IO ()
runTransporterBackendApp' port settings = do
  let commonEnv = CommonEnv Config.defaultDbConfig Config.connectionTag
  let appEnv = AppEnv commonEnv
  let loggerCfg =
        T.defaultLoggerConfig
          { T._logToFile = True,
            T._logFilePath = "/tmp/beckn-transport.log",
            T._isAsync = True
          }
  reqHeadersKey <- V.newKey
  R.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Initializing DB Connections..."
    let prepare = prepareDBConnections
    try (runFlowR flowRt appEnv prepare) >>= \case
      Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
      Right _ -> do
        putStrLn @String "Initializing Redis Connections..."
        try (runFlowR flowRt appEnv prepareRedisConnections) >>= \case
          Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
          Right _ -> do
            putStrLn @String "Initializing Options..."
            try (runFlowR flowRt appEnv prepareAppOptions) >>= \case
              Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
              Right _ ->
                putStrLn @String ("Runtime created. Starting server at port " <> show port)
            runSettings settings $ App.run reqHeadersKey (App.EnvR flowRt appEnv)

transporterExceptionResponse :: SomeException -> Response
transporterExceptionResponse exception = do
  let anyException = fromException exception
  case anyException of
    Just ex ->
      responseLBS
        (H.Status (errHTTPCode ex) (BS.pack $ errReasonPhrase ex))
        ((H.hContentType, "application/json") : errHeaders ex)
        (errBody ex)
    Nothing ->
      responseLBS
        H.internalServerError500
        [(H.hContentType, "application/json")]
        (Aeson.encode internalServerErr)
