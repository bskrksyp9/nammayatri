{-# LANGUAGE OverloadedLabels #-}

module App
  ( runFMDWrapper,
  )
where

import App.Server
import App.Types
import Beckn.Exit
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import qualified Beckn.Types.App as App
import Beckn.Utils.Common
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Migration
import Beckn.Utils.Servant.Server (exceptionResponse)
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Network.Wai (Response)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setOnExceptionResponse,
    setPort,
  )
import System.Environment

runFMDWrapper :: (AppEnv -> AppEnv) -> IO ()
runFMDWrapper configModifier = do
  appEnv <- configModifier <$> readDhallConfigDefault "fmd-wrapper"
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = getEulerLoggerRuntime hostname $ appEnv ^. #loggerConfig
  let settings =
        setOnExceptionResponse fmdWrapperExceptionResponse $
          setPort (port appEnv) defaultSettings
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogContext "Server startup" $ do
        let shortOrgId = appEnv ^. #selfId
        case prepareAuthManager flowRt appEnv "Authorization" shortOrgId of
          Left err -> do
            logError ("Could not prepare authentication manager: " <> show err)
            L.runIO $ exitWith exitAuthManagerPrepFailure
          Right getManager -> do
            authManager <- L.runIO getManager
            try (prepareRedisConnections $ redisCfg appEnv) >>= \case
              Left (e :: SomeException) -> do
                logError ("Exception thrown: " <> show e)
                L.runIO $ exitWith exitRedisConnPrepFailure
              Right _ -> do
                migrateIfNeeded (migrationPath appEnv) (dbCfg appEnv) (autoMigrate appEnv) >>= \case
                  Left e -> do
                    logError ("Couldn't migrate database: " <> show e)
                    L.runIO $ exitWith exitDBMigrationFailure
                  Right _ -> do
                    logInfo ("Runtime created. Starting server at port " <> show (port appEnv))
                    return $ flowRt {R._httpClientManagers = Map.singleton signatureAuthManagerKey authManager}
    runSettings settings $ run $ App.EnvR flowRt' appEnv

fmdWrapperExceptionResponse :: SomeException -> Response
fmdWrapperExceptionResponse = exceptionResponse
