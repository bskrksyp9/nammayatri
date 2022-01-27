module API.UI.Search.Types where

import API.Types.Common (Gps)
import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Search as Domain
import Servant
import Tools.Auth

type API =
  "search"
    :> TokenAuth
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] SearchRes

data SearchReq = SearchReq
  { location :: Gps,
    fromDate :: UTCTime,
    toDate :: UTCTime
  }
  deriving (Generic, FromJSON, ToSchema)

newtype SearchRes = SearchRes
  { searchId :: Id Domain.Search
  }
  deriving (Generic, ToJSON, ToSchema)
