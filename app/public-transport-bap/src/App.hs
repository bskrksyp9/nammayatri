{-# LANGUAGE TypeApplications #-}

module App
  ( runService,
  )
where

import API.Handler
import API.Types
import App.Types
import Beckn.Exit
import Beckn.Prelude
import Beckn.Storage.Redis.Config (prepareRedisConnections)
import Beckn.Types.Flow (FlowR)
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerService)
import Beckn.Utils.Servant.SignatureAuth (modFlowRtWithAuthManagers)
import Servant (Context (..))
import Tools.Auth (verifyPersonAction)

runService :: (AppCfg -> AppCfg) -> IO ()
runService configModifier = do
  appCfg <- readDhallConfigDefault "public-transport-bap" <&> configModifier
  appEnv <- buildAppEnv appCfg
  runServerService appEnv (Proxy @API) handler middleware identity context releaseAppEnv \flowRt -> do
    try (prepareRedisConnections $ appEnv.redisCfg)
      >>= handleLeft @SomeException exitRedisConnPrepFailure "Exception thrown: "
    modFlowRtWithAuthManagers flowRt appEnv [(appCfg.selfId, appCfg.authEntity.uniqueKeyId)]
  where
    middleware =
      hashBodyForSignature
        >>> supportProxyAuthorization
    context = verifyPersonAction @(FlowR AppEnv) :. EmptyContext
