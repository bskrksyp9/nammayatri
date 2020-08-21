{-# LANGUAGE TypeApplications #-}

module App
  ( runFMDWrapper,
  )
where

import App.Server
import App.Types
import Beckn.Constants.APIErrorCode (internalServerErr)
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import qualified Beckn.Types.App as App
import Beckn.Utils.Common (runFlowR)
import Beckn.Utils.Dhall (ZL (Z), readDhallConfigDefault)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vault.Lazy as V
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

runFMDWrapper :: IO ()
runFMDWrapper = do
  appEnv <- readDhallConfigDefault Z "fmd-wrapper"
  let loggerCfg =
        E.defaultLoggerConfig
          { E._logToFile = True,
            E._logFilePath = "/tmp/fmd-wrapper.log",
            E._isAsync = True
          }
  let settings =
        setOnExceptionResponse mockAppExceptionResponse $
          setPort (port appEnv) defaultSettings
  reqHeadersKey <- V.newKey
  E.withFlowRuntime (Just loggerCfg) $ \flowRt -> do
    putStrLn @String "Initializing Redis Connections..."
    try (runFlowR flowRt appEnv $ prepareRedisConnections $ redisCfg appEnv) >>= \case
      Left (e :: SomeException) -> putStrLn @String ("Exception thrown: " <> show e)
      Right _ -> runSettings settings $ run reqHeadersKey (App.EnvR flowRt appEnv)

mockAppExceptionResponse :: SomeException -> Response
mockAppExceptionResponse exception = do
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
