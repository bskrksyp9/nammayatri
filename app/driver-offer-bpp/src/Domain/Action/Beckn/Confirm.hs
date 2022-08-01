module Domain.Action.Beckn.Confirm where

import Beckn.External.Encryption
import qualified Beckn.External.FCM.Types as FCM
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import qualified Beckn.Types.Registry.Subscriber as Subscriber
import Beckn.Utils.Common
import Data.String.Conversions
import qualified Data.Text as T
import Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBL
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as DRD
import Servant.Client (BaseUrl (..))
import Storage.Queries.Booking as QRB
import qualified Storage.Queries.Booking.BookingLocation as QBL
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Utils.Common
import qualified Utils.Notifications as Notify

data DConfirmReq = DConfirmReq
  { bookingId :: Id DRB.Booking,
    customerMobileCountryCode :: Text,
    customerPhoneNumber :: Text,
    fromAddress :: DBL.LocationAddress,
    toAddress :: DBL.LocationAddress
  }

data DConfirmRes = DConfirmRes
  { booking :: DRB.Booking,
    ride :: DRide.Ride,
    fromLocation :: DBL.BookingLocation,
    toLocation :: DBL.BookingLocation,
    riderDetails :: DRD.RiderDetails,
    transporter :: DOrg.Organization
  }

handler ::
  ( FCMFlow m r,
    EsqDBFlow m r,
    HasPrettyLogger m r,
    EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl]
  ) =>
  Subscriber.Subscriber ->
  Id DOrg.Organization ->
  DConfirmReq ->
  m DConfirmRes
handler subscriber transporterId req = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  driverQuote <- QDQ.findById booking.quoteId >>= fromMaybeM (QuoteNotFound booking.quoteId.getId)
  driver <- QPerson.findById driverQuote.driverId >>= fromMaybeM (PersonNotFound driverQuote.driverId.getId)
  let transporterId' = booking.providerId
  transporter <-
    QOrg.findById transporterId'
      >>= fromMaybeM (OrgNotFound transporterId'.getId)
  unless (transporterId' == transporterId) $ throwError AccessDenied
  let bapOrgId = booking.bapId
  unless (subscriber.subscriber_id == bapOrgId) $ throwError AccessDenied
  now <- getCurrentTime
  (riderDetails, isNewRider) <- getRiderDetails req.customerMobileCountryCode req.customerPhoneNumber now
  ride <- buildRide driver.id booking
  Esq.runTransaction $ do
    when isNewRider $ QRD.create riderDetails
    QRB.updateRiderId booking.id riderDetails.id
    QRB.updateStatus booking.id DRB.TRIP_ASSIGNED
    QBL.updateAddress booking.fromLocation.id req.fromAddress
    QBL.updateAddress booking.toLocation.id req.toAddress
    QDI.updateOnRide (cast driver.id) True
    QRide.create ride
    QBE.logRideConfirmedEvent booking.id
    QBE.logDriverAssignedEvent (cast driver.id) booking.id ride.id

  uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
  Notify.notifyDriver notificationType notificationTitle (message uBooking) driver.id driver.deviceToken

  pure
    DConfirmRes
      { booking = uBooking,
        ride,
        riderDetails,
        transporter,
        fromLocation = uBooking.fromLocation,
        toLocation = uBooking.toLocation
      }
  where
    notificationType = FCM.DRIVER_ASSIGNMENT
    notificationTitle = "Driver has been assigned the ride!"
    message booking =
      cs $
        unwords
          [ "You have been assigned a ride for",
            cs (showTimeIst booking.startTime) <> ".",
            "Check the app for more details."
          ]
    buildRide driverId booking = do
      guid <- Id <$> generateGUID
      shortId <- generateShortId
      otp <- generateOTPCode
      now <- getCurrentTime
      trackingUrl <- buildTrackingUrl guid
      return
        DRide.Ride
          { id = guid,
            bookingId = booking.id,
            shortId = shortId,
            status = DRide.NEW,
            driverId = cast driverId,
            otp = otp,
            trackingUrl = trackingUrl,
            fare = Nothing,
            traveledDistance = 0,
            tripStartTime = Nothing,
            tripEndTime = Nothing,
            createdAt = now,
            updatedAt = now
          }
    buildTrackingUrl rideId = do
      bppUIUrl <- asks (.selfUIUrl)
      let rideid = T.unpack (getId rideId)
      return $
        bppUIUrl
          { --TODO: find a way to build it using existing types from Routes
            baseUrlPath = baseUrlPath bppUIUrl <> "/driver/location/" <> rideid
          }

getRiderDetails :: (EncFlow m r, EsqDBFlow m r) => Text -> Text -> UTCTime -> m (DRD.RiderDetails, Bool)
getRiderDetails customerMobileCountryCode customerPhoneNumber now =
  QRD.findByMobileNumber customerPhoneNumber >>= \case
    Nothing -> fmap (,True) . encrypt =<< buildRiderDetails
    Just a -> return (a, False)
  where
    buildRiderDetails = do
      id <- generateGUID
      return $
        DRD.RiderDetails
          { id = id,
            mobileCountryCode = customerMobileCountryCode,
            mobileNumber = customerPhoneNumber,
            createdAt = now,
            updatedAt = now
          }