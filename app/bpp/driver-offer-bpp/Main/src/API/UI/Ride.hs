module API.UI.Ride
  ( StartRideReq (..),
    EndRideReq (..),
    CancelRideReq (..),
    DRide.DriverRideListRes (..),
    DRide.DriverRideRes (..),
    API,
    handler,
  )
where

import Beckn.External.Maps.Types
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.UI.Ride as DRide
import qualified Domain.Action.UI.Ride.CancelRide as RideCancel
import qualified Domain.Action.UI.Ride.EndRide as RideEnd
import qualified Domain.Action.UI.Ride.StartRide as RideStart
import Domain.Types.CancellationReason (CancellationReasonCode (..))
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as Ride
import Environment
import Servant
import SharedLogic.Person (findPerson)
import Tools.Auth

type API =
  "driver" :> "ride"
    :> ( "list"
           :> TokenAuth
           :> QueryParam "limit" Integer
           :> QueryParam "offset" Integer
           :> QueryParam "onlyActive" Bool
           :> Get '[JSON] DRide.DriverRideListRes
           :<|> TokenAuth
           :> Capture "rideId" (Id Ride.Ride)
           :> "arrived"
           :> "pickup"
           :> ReqBody '[JSON] LatLong
           :> Post '[JSON] APISuccess
           :<|> TokenAuth
           :> Capture "rideId" (Id Ride.Ride)
           :> "start"
           :> ReqBody '[JSON] StartRideReq
           :> Post '[JSON] APISuccess
           :<|> TokenAuth
           :> Capture "rideId" (Id Ride.Ride)
           :> "end"
           :> ReqBody '[JSON] EndRideReq
           :> Post '[JSON] APISuccess
           :<|> TokenAuth
           :> Capture "rideId" (Id Ride.Ride)
           :> "cancel"
           :> ReqBody '[JSON] CancelRideReq
           :> Post '[JSON] APISuccess
       )

data StartRideReq = StartRideReq
  { rideOtp :: Text,
    point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype EndRideReq = EndRideReq
  { point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

handler :: FlowServer API
handler =
  listDriverRides
    :<|> arrivedAtPickup
    :<|> startRide
    :<|> endRide
    :<|> cancelRide

startRide :: Id SP.Person -> Id Ride.Ride -> StartRideReq -> FlowHandler APISuccess
startRide requestorId rideId StartRideReq {rideOtp, point} = withFlowHandlerAPI $ do
  requestor <- findPerson requestorId
  let driverReq = RideStart.DriverStartRideReq {rideOtp, point, requestor}
  shandle <- RideStart.buildStartRideHandle requestor.merchantId
  RideStart.driverStartRide shandle rideId driverReq

endRide :: Id SP.Person -> Id Ride.Ride -> EndRideReq -> FlowHandler APISuccess
endRide requestorId rideId EndRideReq {point} = withFlowHandlerAPI $ do
  requestor <- findPerson requestorId
  let driverReq = RideEnd.DriverEndRideReq {point, requestor}
  shandle <- RideEnd.buildEndRideHandle requestor.merchantId
  RideEnd.driverEndRide shandle rideId driverReq

cancelRide :: Id SP.Person -> Id Ride.Ride -> CancelRideReq -> FlowHandler APISuccess
cancelRide personId rideId CancelRideReq {reasonCode, additionalInfo} = withFlowHandlerAPI $ do
  let driverReq = RideCancel.CancelRideReq {reasonCode, additionalInfo}
  RideCancel.driverCancelRideHandler RideCancel.cancelRideHandle personId rideId driverReq

listDriverRides ::
  Id SP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  FlowHandler DRide.DriverRideListRes
listDriverRides driverId mbLimit mbOffset = withFlowHandlerAPI . DRide.listDriverRides driverId mbLimit mbOffset

arrivedAtPickup :: Id SP.Person -> Id Ride.Ride -> LatLong -> FlowHandler APISuccess
arrivedAtPickup _ rideId req = withFlowHandlerAPI $ DRide.arrivedAtPickup rideId req