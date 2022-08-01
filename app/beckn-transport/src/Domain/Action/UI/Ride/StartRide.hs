module Domain.Action.UI.Ride.StartRide where

import Beckn.Prelude (ToSchema)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude
import Types.Error
import Utils.Common

data ServiceHandle m = ServiceHandle
  { findById :: Id Person.Person -> m (Maybe Person.Person),
    findBookingById :: Id SRB.Booking -> m (Maybe SRB.Booking),
    findRideById :: Id SRide.Ride -> m (Maybe SRide.Ride),
    startRideAndUpdateLocation :: Id SRide.Ride -> Id SRB.Booking -> Id Person.Person -> LatLong -> m (),
    notifyBAPRideStarted :: SRB.Booking -> SRide.Ride -> m (),
    rateLimitStartRide :: Id Person.Person -> Id SRide.Ride -> m (),
    addFirstWaypoint :: Id Person.Person -> LatLong -> m ()
  }

data StartRideReq = StartRideReq
  { rideOtp :: Text,
    point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

startRideHandler :: (MonadThrow m, Log m) => ServiceHandle m -> Id Person.Person -> Id SRide.Ride -> StartRideReq -> m APISuccess.APISuccess
startRideHandler ServiceHandle {..} driverId rideId req = do
  rateLimitStartRide driverId rideId
  requestor <- findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  ride <- findRideById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  case requestor.role of
    Person.DRIVER -> do
      let rideDriver = ride.driverId
      unless (rideDriver == driverId) $ throwError NotAnExecutor
    _ -> throwError AccessDenied
  unless (isValidRideStatus (ride.status)) $ throwError $ RideInvalidStatus "This ride cannot be started"
  booking <- findBookingById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let inAppOtp = ride.otp
  when (req.rideOtp /= inAppOtp) $ throwError IncorrectOTP
  logTagInfo "startRide" ("DriverId " <> getId driverId <> ", RideId " <> getId rideId)
  startRideAndUpdateLocation ride.id booking.id driverId req.point
  addFirstWaypoint driverId req.point
  notifyBAPRideStarted booking ride
  pure APISuccess.Success
  where
    isValidRideStatus status = status == SRide.NEW