module MockProviderBackend.Update where

import Data.Time
import EulerHS.Prelude
import MockProviderBackend.Fixtures
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec
import Utils

spec :: Spec
spec = do
  mockProviderManager <- runIO $ Client.newManager tlsManagerSettings
  let providerClientEnv = mkClientEnv mockProviderManager mockProviderBaseUrl
  describe "Mock Provider Backend Update Api" $
    it "should return valid ack response" do
      now <- getCurrentTime
      let ctx = buildContext "update" "dummy-txn-id" now
          updateReq = buildFMDUpdateReq ctx
      initiateUpdateRes <- runClient providerClientEnv $ updateFlow mockProviderApiKey updateReq
      initiateUpdateRes `shouldSatisfy` isRight
