module Product.BecknProvider.BP
  ( sendRideAssignedUpdateToBAP,
    sendRideStartedUpdateToBAP,
    sendRideCompletedUpdateToBAP,
    {-
      sendRideBookingCancelledUpdateToBAP,
      -}
  )
where

import Beckn.Types.Common
--import Beckn.Types.Id
import qualified Core.ACL.OnUpdate as ACL
--import qualified Domain.Types.Organization as SOrg

--import qualified Domain.Types.RideBookingCancellationReason as SRBCR

import qualified Domain.Types.FareParams as Fare
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import EulerHS.Prelude
import ExternalAPI.Flow (callOnUpdate)
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Metrics (CoreMetrics)
import Types.Error
import Utils.Common

sendRideAssignedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  m ()
sendRideAssignedUpdateToBAP rideBooking ride = do
  transporter <-
    QOrg.findById rideBooking.providerId
      >>= fromMaybeM (OrgNotFound rideBooking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  let rideAssignedBuildReq = ACL.RideAssignedBuildReq {..}
  rideAssignedMsg <- ACL.buildOnUpdateMessage rideAssignedBuildReq
  void $ callOnUpdate transporter rideBooking rideAssignedMsg

sendRideStartedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  m ()
sendRideStartedUpdateToBAP rideBooking ride = do
  transporter <-
    QOrg.findById rideBooking.providerId
      >>= fromMaybeM (OrgNotFound rideBooking.providerId.getId)
  let rideStartedBuildReq = ACL.RideStartedBuildReq {..}
  rideStartedMsg <- ACL.buildOnUpdateMessage rideStartedBuildReq
  void $ callOnUpdate transporter rideBooking rideStartedMsg

sendRideCompletedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SRide.Ride ->
  Fare.FareParameters ->
  m ()
sendRideCompletedUpdateToBAP rideBooking ride fareParams = do
  transporter <-
    QOrg.findById rideBooking.providerId
      >>= fromMaybeM (OrgNotFound rideBooking.providerId.getId)
  let rideCompletedBuildReq = ACL.RideCompletedBuildReq {ride, fareParams}
  rideCompletedMsg <- ACL.buildOnUpdateMessage rideCompletedBuildReq
  void $ callOnUpdate transporter rideBooking rideCompletedMsg

{-
sendRideBookingCancelledUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  SOrg.Organization ->
  SRBCR.CancellationSource ->
  m ()
sendRideBookingCancelledUpdateToBAP booking transporter cancellationSource = do
  let bookingCancelledBuildReq = ACL.BookingCancelledBuildReq {..}
  bookingCancelledMsg <- ACL.buildOnUpdateMessage bookingCancelledBuildReq
  void $ callOnUpdate transporter booking bookingCancelledMsg
-}
