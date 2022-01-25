module FerryBppApp
  ( runMockFerryBPP,
  )
where

import API.Confirm
import API.HealthCheck
import API.Search
import API.Status
import API.Types
import Beckn.Mock.App
import Beckn.Mock.Environment
import Beckn.Mock.Redis (withRedisConnection)
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
  )
import Relude
import Servant

runMockFerryBPP :: IO ()
runMockFerryBPP = do
  appCfg <- readDhallConfigDefault "mock-public-transport-bpp" :: IO AppCfg
  withRedisConnection $ \redisCon -> do
    let port = appCfg.port
        appEnv = buildAppEnv redisCon appCfg
        settings =
          defaultSettings & setPort port
    runSettings settings $
      run totalAPI totalServer appEnv

totalServer :: ServerT TotalAPI (MockM AppEnv)
totalServer = healthCheckServer :<|> searchServer :<|> confirmServer :<|> statusServer
