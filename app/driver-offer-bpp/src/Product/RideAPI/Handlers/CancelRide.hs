module Product.RideAPI.Handlers.CancelRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude
import Types.API.Ride (CancelRideReq (..))
import Types.Error
import Utils.Common

type MonadHandler m = (MonadThrow m, Log m, MonadGuid m)

data ServiceHandle m = ServiceHandle
  { findRideById :: Id SRide.Ride -> m (Maybe SRide.Ride),
    findById :: Id Person.Person -> m (Maybe Person.Person),
    cancelRide :: Id SRide.Ride -> SBCR.BookingCancellationReason -> m ()
  }

cancelRideHandler :: MonadHandler m => ServiceHandle m -> Id Person.Person -> Id SRide.Ride -> CancelRideReq -> m APISuccess.APISuccess
cancelRideHandler ServiceHandle {..} personId rideId req = do
  ride <- findRideById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (isValidRide ride) $ throwError $ RideInvalidStatus "This ride cannot be canceled"
  authPerson <-
    findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  rideCancelationReason <- case authPerson.role of
    Person.ADMIN -> do
      buildRideCancelationReason Nothing SBCR.ByOrganization ride
    Person.DRIVER -> do
      let driverId = ride.driverId
      unless (authPerson.id == driverId) $ throwError NotAnExecutor
      buildRideCancelationReason (Just driverId) SBCR.ByDriver ride
  cancelRide rideId rideCancelationReason
  pure APISuccess.Success
  where
    isValidRide ride =
      ride.status == SRide.NEW
    buildRideCancelationReason mbDriverId source ride = do
      let CancelRideReq {..} = req
      guid <- generateGUID
      return $
        SBCR.BookingCancellationReason
          { id = guid,
            bookingId = ride.bookingId,
            rideId = Just ride.id,
            source = source,
            reasonCode = Just reasonCode,
            driverId = mbDriverId,
            ..
          }