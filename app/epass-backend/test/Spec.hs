import Beckn.App
import Beckn.App.Routes
import qualified Beckn.App.Server as App
import Beckn.Types.App
import Data.Text.Encoding as DT
import qualified Data.Vault.Lazy as V
import EulerHS.Interpreters
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Runtime
import qualified EulerHS.Types as T
import Network.HTTP.Client hiding (Proxy)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant.Client
import Test.Hspec
import qualified Test.Hspec.Core.Spec as CS
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  reqHeadersKey <- CS.runIO V.newKey
  baseUrl <- CS.runIO $ parseBaseUrl "http://localhost"
  let mSetting = tlsManagerSettings
  --{Client.managerModifyRequest = makeRequestByAddingHeaders Nothing}
  manager <- CS.runIO $ Client.newManager mSetting
  let clientEnv port = ClientEnv manager (baseUrl {baseUrlPort = port}) Nothing
  let loggerCfg =
        T.defaultLoggerConfig
          { T._logToFile = True,
            T._logFilePath = "/tmp/newton-backend.log",
            T._isAsync = False
          }
  around (withFlowRuntime (Just loggerCfg)) $ do
    describe "Testing api's" $ do
      it "starting server" $ \flowRt -> do
        let env = Env flowRt
        hspec $ do
          it "preparedbconnections" $ do
            conE <- try $ runFlow flowRt prepareDBConnections
            case conE of
              Right v -> 1 `shouldBe` 1
              Left (err :: SomeException) -> print err *> (2 `shouldBe` 1)
        hspec
          $ around (withBecknApp reqHeadersKey $ env)
          $ do
            it "health check api should return success" $ \port -> do
              result <- runClientM healthAppC (clientEnv port)
              result `shouldBe` (Right $ DT.decodeUtf8 "App is UP")

healthAppC = client epassAPIs

withBecknApp reqHeadersKey env =
  Warp.testWithApplication (pure $ App.run reqHeadersKey env)
