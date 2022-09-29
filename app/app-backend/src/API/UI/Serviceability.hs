module API.UI.Serviceability
  ( API,
    handler,
    ServiceabilityReq (..),
    ServiceabilityRes (..),
  )
where

import App.Types
import Beckn.Prelude
import Beckn.Product.MapSearch.PolyLinePoints
import Beckn.Types.Geofencing
import Beckn.Types.Id
import qualified Domain.Action.UI.Serviceability as DServiceability
import Domain.Types.Person as Person
import Servant
import Utils.Auth
import Utils.Common

-------- Serviceability----------
type API =
  "serviceability"
    :> TokenAuth
    :> ( "origin"
           :> ReqBody '[JSON] ServiceabilityReq
           :> Post '[JSON] ServiceabilityRes
           :<|> "destination"
             :> ReqBody '[JSON] ServiceabilityReq
             :> Post '[JSON] ServiceabilityRes
       )

newtype ServiceabilityReq = ServiceabilityReq
  { location :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype ServiceabilityRes = ServiceabilityRes
  { serviceable :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

handler :: FlowServer API
handler regToken =
  checkServiceability origin regToken
    :<|> checkServiceability destination regToken

checkServiceability ::
  (GeofencingConfig -> GeoRestriction) ->
  Id Person.Person ->
  ServiceabilityReq ->
  FlowHandler ServiceabilityRes
checkServiceability settingAccessor personId ServiceabilityReq {..} = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  ServiceabilityRes <$> DServiceability.checkServiceability settingAccessor personId location