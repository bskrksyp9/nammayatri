{-# LANGUAGE OverloadedLabels #-}

module SharedLogic.CallBAP
  ( sendRideAssignedUpdateToBAP,
    sendRideStartedUpdateToBAP,
    sendRideCompletedUpdateToBAP,
    sendBookingCancelledUpdateToBAP,
    sendBookingReallocationUpdateToBAP,
    buildBppUrl,
  )
where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes (BecknCallbackReq (BecknCallbackReq))
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as API
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnUpdate as ACL
import qualified Data.Text as T
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SRBCR
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import Domain.Types.Organization as Org
import qualified Domain.Types.Organization as SOrg
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error
import Tools.Metrics (CoreMetrics)

sendRideAssignedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
sendRideAssignedUpdateToBAP booking ride = do
  transporter <-
    QOrg.findById booking.providerId
      >>= fromMaybeM (OrgNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  let rideAssignedBuildReq = ACL.RideAssignedBuildReq {..}
  rideAssignedMsg <- ACL.buildOnUpdateMessage rideAssignedBuildReq
  void $ callOnUpdate transporter booking rideAssignedMsg

sendRideStartedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
sendRideStartedUpdateToBAP booking ride = do
  transporter <-
    QOrg.findById booking.providerId
      >>= fromMaybeM (OrgNotFound booking.providerId.getId)
  let rideStartedBuildReq = ACL.RideStartedBuildReq {..}
  rideStartedMsg <- ACL.buildOnUpdateMessage rideStartedBuildReq
  void $ callOnUpdate transporter booking rideStartedMsg

sendRideCompletedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  [DFareBreakup.FareBreakup] ->
  m ()
sendRideCompletedUpdateToBAP booking ride fareBreakups = do
  transporter <-
    QOrg.findById booking.providerId
      >>= fromMaybeM (OrgNotFound booking.providerId.getId)
  let rideCompletedBuildReq = ACL.RideCompletedBuildReq {..}
  rideCompletedMsg <- ACL.buildOnUpdateMessage rideCompletedBuildReq
  void $ callOnUpdate transporter booking rideCompletedMsg

sendBookingCancelledUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SOrg.Organization ->
  SRBCR.CancellationSource ->
  m ()
sendBookingCancelledUpdateToBAP booking transporter cancellationSource = do
  let bookingCancelledBuildReq = ACL.BookingCancelledBuildReq {..}
  bookingCancelledMsg <- ACL.buildOnUpdateMessage bookingCancelledBuildReq
  void $ callOnUpdate transporter booking bookingCancelledMsg

sendBookingReallocationUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.Booking ->
  Id SRide.Ride ->
  SOrg.Organization ->
  m ()
sendBookingReallocationUpdateToBAP booking rideId transporter = do
  let bookingReallocationBuildReq = ACL.BookingReallocationBuildReq {..}
  bookingReallocationMsg <- ACL.buildOnUpdateMessage bookingReallocationBuildReq
  void $ callOnUpdate transporter booking bookingReallocationMsg

callOnUpdate ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Org.Organization ->
  SRB.Booking ->
  OnUpdate.OnUpdateMessage ->
  m ()
callOnUpdate transporter booking content = do
  let bapId = booking.bapId
      bapUri = booking.bapUri
  let bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
  bppUri <- buildBppUrl transporter.id
  msgId <- generateGUID
  context <- buildTaxiContext Context.ON_UPDATE msgId Nothing bapId bapUri (Just transporter.shortId.getShortId) (Just bppUri)
  void . Beckn.callBecknAPI (Just authKey) Nothing (show Context.ON_UPDATE) API.onUpdateAPI bapUri . BecknCallbackReq context $ Right content

buildBppUrl ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Id Org.Organization ->
  m BaseUrl
buildBppUrl (Id transporterId) =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack transporterId)