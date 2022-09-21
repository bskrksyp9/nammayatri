module API.UI.Ride
  ( API,
    handler,
    DRide.GetDriverLocRes,
  )
where

import App.Types
import Beckn.Types.Id
import Data.Aeson.Types ()
import qualified Domain.Action.UI.Ride as DRide
import qualified Domain.Types.Person as SPerson
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude hiding (id)
import Servant
import Utils.Auth
import Utils.Common

type API =
  "ride"
    :> Capture "rideId" (Id SRide.Ride)
    :> "driver"
    :> "location"
    :> TokenAuth
    :> Post '[JSON] DRide.GetDriverLocRes

handler :: FlowServer API
handler = getDriverLoc

getDriverLoc :: Id SRide.Ride -> Id SPerson.Person -> FlowHandler DRide.GetDriverLocRes
getDriverLoc rideId personId = withFlowHandlerAPI . withPersonIdLogTag personId $ DRide.getDriverLoc rideId personId
