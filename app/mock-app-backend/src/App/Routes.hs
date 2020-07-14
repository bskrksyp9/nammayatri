module App.Routes where

import qualified Beckn.Types.API.Search as Search
import Beckn.Types.App (FlowServer)
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified Product.Search as P
import Servant

type MockAppBackendAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> TriggerSearchAPI
           :<|> OnSearchAPI
       )

mockAppBackendAPI :: Proxy MockAppBackendAPI
mockAppBackendAPI = Proxy

mockAppBackendServer :: V.Key (HashMap Text Text) -> FlowServer MockAppBackendAPI
mockAppBackendServer _key =
  pure "Mock app backend is UP"
    :<|> triggerSearchFlow
    :<|> onSearchFlow

type TriggerSearchAPI =
  "trigger"
    :> Get '[JSON] Search.SearchRes

type OnSearchAPI = Search.OnSearchAPI

onSearchFlow :: FlowServer OnSearchAPI
onSearchFlow = P.searchCb

triggerSearchFlow :: FlowServer TriggerSearchAPI
triggerSearchFlow = P.triggerSearch
