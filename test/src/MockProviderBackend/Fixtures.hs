module MockProviderBackend.Fixtures where

import Beckn.Types.Core.Ack
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.FMD.API.Update
import EulerHS.Prelude
import Servant.Client

mockProviderBaseUrl :: BaseUrl
mockProviderBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8017,
      baseUrlPath = "/v1"
    }

searchFlow :: Text -> SearchReq -> ClientM AckResponse
searchFlow = client searchAPI

selectFlow :: Text -> SelectReq -> ClientM AckResponse
selectFlow = client selectAPI

initFlow :: Text -> InitReq -> ClientM AckResponse
initFlow = client initAPI

confirmFlow :: Text -> ConfirmReq -> ClientM AckResponse
confirmFlow = client confirmAPI

updateFlow :: Text -> UpdateReq -> ClientM AckResponse
updateFlow = client updateAPI

mockProviderApiKey :: Text
mockProviderApiKey = "test-provider-2-key"
