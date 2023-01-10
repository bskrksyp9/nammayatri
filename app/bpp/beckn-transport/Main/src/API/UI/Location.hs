module API.UI.Location (module Reexport, API, handler) where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Id
import Beckn.Utils.Common hiding (id)
import Domain.Action.UI.Location as Reexport
  ( GetLocationRes (..),
    Status (..),
  )
import qualified Domain.Action.UI.Location as DLocation
import Domain.Action.UI.Location.UpdateLocation as Reexport
  ( UpdateLocationReq,
    UpdateLocationRes,
    Waypoint (..),
  )
import qualified Domain.Action.UI.Location.UpdateLocation as DLocation
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Environment
import Servant
import Tools.Auth (TokenAuth)

-- Location update and get for tracking is as follows
type API =
  "driver" :> "location"
    :> ( Capture "rideId" (Id SRide.Ride) -- TODO: add auth
           :> Get '[JSON] GetLocationRes
           :<|> TokenAuth
           :> ReqBody '[JSON] UpdateLocationReq
           :> Post '[JSON] UpdateLocationRes
       )

handler :: FlowServer API
handler =
  getLocation
    :<|> updateLocation

updateLocation :: Id Person.Person -> UpdateLocationReq -> FlowHandler APISuccess
updateLocation personId waypoints = withFlowHandlerAPI $ do
  hdlr <- DLocation.buildUpdateLocationHandle personId
  DLocation.updateLocationHandler hdlr waypoints

getLocation :: Id SRide.Ride -> FlowHandler GetLocationRes
getLocation = withFlowHandlerAPI . DLocation.getLocation