module App.Handlers where

import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..))
import Beckn.Types.FMD.API.Cancel (CancelReq)
import Beckn.Types.FMD.API.Confirm (ConfirmReq)
import Beckn.Types.FMD.API.Init (InitReq)
import Beckn.Types.FMD.API.Search (SearchReq)
import Beckn.Types.FMD.API.Select (SelectReq)
import Beckn.Types.FMD.API.Status (StatusReq)
import Beckn.Types.FMD.API.Update (UpdateReq)
import Beckn.Utils.Servant.HeaderAuth
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import qualified Product.Cancel as P
import qualified Product.Confirm as P
import qualified Product.Init as P
import qualified Product.Search as P
import qualified Product.Select as P
import qualified Product.Status as P
import qualified Product.Update as P
import Servant
import Utils.Auth

type ProviderAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> ProviderSearchAPI
           :<|> ProviderSelectAPI
           :<|> ProviderInitAPI
           :<|> ProviderConfirmAPI
           :<|> ProviderStatusAPI
           :<|> ProviderCancelAPI
           :<|> ProviderUpdateAPI
       )

providerAPI :: Proxy ProviderAPI
providerAPI = Proxy

type ProviderSearchAPI =
  "search"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

type ProviderSelectAPI =
  "select"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] SelectReq
    :> Post '[JSON] AckResponse

type ProviderInitAPI =
  "init"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] InitReq
    :> Post '[JSON] AckResponse

type ProviderConfirmAPI =
  "confirm"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] ConfirmReq
    :> Post '[JSON] AckResponse

type ProviderStatusAPI =
  "status"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] StatusReq
    :> Post '[JSON] AckResponse

type ProviderCancelAPI =
  "cancel"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] CancelReq
    :> Post '[JSON] AckResponse

type ProviderUpdateAPI =
  "update"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] UpdateReq
    :> Post '[JSON] AckResponse

mockProviderBackendServer :: V.Key (HashMap Text Text) -> FlowServerR r ProviderAPI
mockProviderBackendServer _key =
  pure "Mock provider backend is UP"
    :<|> searchFlow
    :<|> selectFlow
    :<|> initFlow
    :<|> confirmFlow
    :<|> statusFlow
    :<|> cancelFlow
    :<|> updateFlow

searchFlow :: FlowServerR r ProviderSearchAPI
searchFlow = P.search

selectFlow :: FlowServerR r ProviderSelectAPI
selectFlow = P.select

initFlow :: FlowServerR r ProviderInitAPI
initFlow = P.init

confirmFlow :: FlowServerR r ProviderConfirmAPI
confirmFlow = P.confirm

statusFlow :: FlowServerR r ProviderStatusAPI
statusFlow = P.status

cancelFlow :: FlowServerR r ProviderCancelAPI
cancelFlow = P.cancel

updateFlow :: FlowServerR r ProviderUpdateAPI
updateFlow = P.update
