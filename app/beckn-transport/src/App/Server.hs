{-# LANGUAGE TypeApplications #-}

module App.Server where

import App.Routes (transporterAPI, transporterServer)
import App.Types
import Beckn.Types.Flow
import Beckn.Utils.App
import Beckn.Utils.Monitoring.Prometheus.Metrics (addServantInfo)
import qualified Beckn.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: Env -> Application
run = withModifiedEnv $ \modifiedEnv ->
  BU.run transporterAPI transporterServer context modifiedEnv
    & logRequestAndResponse modifiedEnv
    & addServantInfo transporterAPI
    & hashBodyForSignature
  where
    context =
      verifyApiKey @(FlowR AppEnv)
        :. verifyTokenAction @(FlowR AppEnv)
        :. validateAdminAction @(FlowR AppEnv)
        :. validateDriverAction @(FlowR AppEnv)
        :. EmptyContext
