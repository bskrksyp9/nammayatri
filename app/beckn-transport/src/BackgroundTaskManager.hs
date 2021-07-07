{-# LANGUAGE TypeApplications #-}

module BackgroundTaskManager where

import App.BackgroundTaskManager.Routes
import App.BackgroundTaskManager.Types
import Beckn.Exit
import Beckn.Storage.Common
import Beckn.Storage.Redis.Config
import qualified Beckn.Types.App as App
import Beckn.Types.Id
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import qualified Beckn.Utils.Servant.Server as Server
import Beckn.Utils.Servant.SignatureAuth
import Control.Concurrent
import qualified Data.Text as T
import EulerHS.Prelude hiding (exitSuccess)
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
import Servant
import qualified Services.Allocation.Runner as Runner
import qualified Storage.Queries.Organization as Storage
import System.Environment
import qualified Types.Storage.Organization as Organization
import Utils.Common
import qualified Utils.Metrics as Metrics

runBackgroundTaskManager :: (BTMCfg -> BTMCfg) -> IO ()
runBackgroundTaskManager configModifier = do
  btmCfg <- configModifier <$> readDhallConfigDefault "beckn-transport-btm"
  Metrics.serve (btmCfg.metricsPort)
  let appCfg = btmCfg.appCfg
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = getEulerLoggerRuntime hostname $ appCfg.loggerConfig
  let redisCfg = appCfg.redisCfg
  let checkConnections = prepareRedisConnections redisCfg >> prepareDBConnections
  let port = appCfg.bgtmPort
  btmEnv <- buildBTMEnv btmCfg

  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt btmEnv $ do
      withLogTag "BTM startup" $ do
        _ <-
          try checkConnections
            >>= handleLeft @SomeException exitConnCheckFailure "Connections check failed. Exception thrown: "
        logInfo "Setting up for signature auth..."
        allProviders <-
          try Storage.loadAllProviders
            >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        let allShortIds = map (getShortId . Organization.shortId) allProviders
        managersSettings <-
          prepareAuthManagers flowRt btmEnv allShortIds
            & handleLeft exitAuthManagerPrepFailure "Could not prepare authentication managers: "
        managers <- createManagers managersSettings
        logInfo ("Loaded http managers - " <> show (keys managers))
        logInfo $ "Starting Background Task Manager on port " <> show port
        return $ flowRt {R._httpClientManagers = managers}
    let settings =
          defaultSettings
            & setGracefulShutdownTimeout (Just $ appCfg.graceTerminationPeriod)
            & setInstallShutdownHandler (handleShutdown $ btmEnv.isShuttingDown)
            & setPort port
    void . forkIO . runSettings settings $ Server.run healthCheckAPI healthCheckServer EmptyContext (App.EnvR flowRt' btmEnv)
    runFlowR flowRt' btmEnv Runner.run
