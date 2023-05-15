{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Confirm where

import Data.String.Conversions
import qualified Data.Text as T
import Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBL
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as SRD
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.SearchRequestForDriver as SReqD
import Kernel.External.Encryption
import qualified Kernel.External.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Servant.Client (BaseUrl (..))
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.DriverLocation as DLoc
import qualified SharedLogic.DriverMode as DMode
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.Ride as SRide
import Storage.CachedQueries.CacheConfig
import Storage.CachedQueries.Merchant as QM
import Storage.Queries.Booking as QRB
import qualified Storage.Queries.Booking.BookingLocation as QBL
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.QuoteSpecialZone as QQSpecialZone
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideD
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Storage.Queries.Vehicle as QVeh
import qualified Tools.Notifications as Notify

data DConfirmReq = DConfirmReq
  { bookingId :: Id DRB.Booking,
    customerMobileCountryCode :: Text,
    customerPhoneNumber :: Text,
    fromAddress :: DBL.LocationAddress,
    toAddress :: DBL.LocationAddress,
    mbRiderName :: Maybe Text
  }

data DConfirmRes = DConfirmRes
  { booking :: DRB.Booking,
    ride :: Maybe DRide.Ride,
    fromLocation :: DBL.BookingLocation,
    toLocation :: DBL.BookingLocation,
    riderDetails :: DRD.RiderDetails,
    transporter :: DM.Merchant
  }

handler ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HedisFlow m r,
    HasPrettyLogger m r,
    HasHttpClientOptions r c,
    EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasLongDurationRetryCfg r c
  ) =>
  Subscriber.Subscriber ->
  Id DM.Merchant ->
  DConfirmReq ->
  m DConfirmRes
handler subscriber transporterId req = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  case booking.bookingType of
    DRB.NormalBooking -> do
      driverQuote <- QDQ.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
      driver <- QPerson.findById driverQuote.driverId >>= fromMaybeM (PersonNotFound driverQuote.driverId.getId)
      let transporterId' = booking.providerId
      transporter <-
        QM.findById transporterId'
          >>= fromMaybeM (MerchantNotFound transporterId'.getId)
      unless (transporterId' == transporterId) $ throwError AccessDenied
      now <- getCurrentTime
      unless (driverQuote.validTill > now || driverQuote.status == DDQ.Active) $ do
        cancelBooking booking (Just driver) transporter
        throwError $ QuoteExpired driverQuote.id.getId
      let bapMerchantId = booking.bapId
      unless (subscriber.subscriber_id == bapMerchantId) $ throwError AccessDenied
      (riderDetails, isNewRider) <- getRiderDetails transporter.id req.customerMobileCountryCode req.customerPhoneNumber now
      ride <- buildRide driver.id booking
      rideDetails <- buildRideDetails ride driver
      driverSearchReqs <- QSRD.findAllActiveBySRId driverQuote.searchRequestId
      Esq.runTransaction $ do
        when isNewRider $ QRD.create riderDetails
        QRB.updateRiderId booking.id riderDetails.id
        QRB.updateStatus booking.id DRB.TRIP_ASSIGNED
        QBL.updateAddress booking.fromLocation.id req.fromAddress
        QBL.updateAddress booking.toLocation.id req.toAddress
        whenJust req.mbRiderName $ QRB.updateRiderName booking.id
        QRide.create ride
        QDFS.updateStatus driver.id DDFS.RIDE_ASSIGNED {rideId = ride.id}
        QRideD.create rideDetails
        QBE.logRideConfirmedEvent booking.id
        QBE.logDriverAssignedEvent (cast driver.id) booking.id ride.id
        QDQ.setInactiveByRequestId driverQuote.searchRequestId
        QSRD.setInactiveBySRId driverQuote.searchRequestId
      DLoc.updateOnRide (cast driver.id) True

      for_ driverSearchReqs $ \driverReq -> do
        let driverId = driverReq.driverId
        unless (driverId == driver.id) $ do
          DP.decrementTotalQuotesCount transporter.id (cast driverReq.driverId) driverReq.searchRequestId
          DP.removeSearchReqIdFromMap transporter.id driverId driverReq.searchRequestId
          Esq.runTransaction $ do
            QSRD.updateDriverResponse driverReq.id SReqD.Pulled
          driver_ <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
          Notify.notifyDriverClearedFare transporter.id driverId driverReq.searchRequestId driverQuote.estimatedFare driver_.deviceToken

      uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
      Notify.notifyDriver transporter.id notificationType notificationTitle (message uBooking) driver.id driver.deviceToken

      pure
        DConfirmRes
          { booking = uBooking,
            ride = Just ride,
            riderDetails,
            transporter,
            fromLocation = uBooking.fromLocation,
            toLocation = uBooking.toLocation
          }
    DRB.SpecialZoneBooking -> do
      quoteSpecialZone <- QQSpecialZone.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
      let transporterId' = booking.providerId
      transporter <-
        QM.findById transporterId'
          >>= fromMaybeM (MerchantNotFound transporterId'.getId)
      unless (transporterId' == transporterId) $ throwError AccessDenied
      now <- getCurrentTime
      unless (quoteSpecialZone.validTill > now) $ do
        cancelBooking booking Nothing transporter
        throwError $ QuoteExpired quoteSpecialZone.id.getId
      let bapMerchantId = booking.bapId
      unless (subscriber.subscriber_id == bapMerchantId) $ throwError AccessDenied
      (riderDetails, isNewRider) <- getRiderDetails transporter.id req.customerMobileCountryCode req.customerPhoneNumber now
      otpCode <- generateOTPCode
      Esq.runTransaction $ do
        when isNewRider $ QRD.create riderDetails
        QRB.updateRiderId booking.id riderDetails.id
        QRB.updateSpecialZoneOtpCode booking.id otpCode
        QBL.updateAddress booking.fromLocation.id req.fromAddress
        QBL.updateAddress booking.toLocation.id req.toAddress
        whenJust req.mbRiderName $ QRB.updateRiderName booking.id
        QBE.logRideConfirmedEvent booking.id
      uBooking <- QRB.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)

      pure
        DConfirmRes
          { booking = uBooking,
            ride = Nothing,
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
            chargeableDistance = Nothing,
            driverArrivalTime = Nothing,
            tripStartTime = Nothing,
            tripEndTime = Nothing,
            tripStartPos = Nothing,
            tripEndPos = Nothing,
            fareParametersId = Nothing,
            distanceCalculationFailed = Nothing,
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

getRiderDetails :: (EncFlow m r, EsqDBFlow m r) => Id DM.Merchant -> Text -> Text -> UTCTime -> m (DRD.RiderDetails, Bool)
getRiderDetails merchantId customerMobileCountryCode customerPhoneNumber now =
  QRD.findByMobileNumberAndMerchant customerPhoneNumber merchantId >>= \case
    Nothing -> fmap (,True) . encrypt =<< buildRiderDetails
    Just a -> return (a, False)
  where
    buildRiderDetails = do
      id <- generateGUID
      return $
        DRD.RiderDetails
          { id = id,
            mobileCountryCode = customerMobileCountryCode,
            merchantId,
            mobileNumber = customerPhoneNumber,
            createdAt = now,
            updatedAt = now,
            referralCode = Nothing,
            referredByDriver = Nothing,
            referredAt = Nothing,
            hasTakenValidRide = False,
            hasTakenValidRideAt = Nothing
          }

buildRideDetails ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    HasPrettyLogger m r,
    EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["selfUIUrl" ::: BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  DRide.Ride ->
  DPerson.Person ->
  m SRD.RideDetails
buildRideDetails ride driver = do
  vehicle <-
    QVeh.findById ride.driverId
      >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  return
    SRD.RideDetails
      { id = ride.id,
        driverName = driver.firstName,
        driverNumber = driver.mobileNumber,
        driverCountryCode = driver.mobileCountryCode,
        vehicleNumber = vehicle.registrationNo,
        vehicleColor = Just vehicle.color,
        vehicleVariant = Just vehicle.variant,
        vehicleModel = Just vehicle.model,
        vehicleClass = Nothing
      }

cancelBooking ::
  ( EsqDBFlow m r,
    HedisFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    CoreMetrics m,
    HasLongDurationRetryCfg r c,
    HasCacheConfig r
  ) =>
  DRB.Booking ->
  Maybe DPerson.Person ->
  DM.Merchant ->
  m ()
cancelBooking booking mbDriver transporter = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCR.ByApplication)
  let transporterId' = booking.providerId
  unless (transporterId' == transporter.id) $ throwError AccessDenied
  mbRide <- QRide.findActiveByRBId booking.id
  bookingCancellationReason <- case mbDriver of
    Nothing -> buildBookingCancellationReason booking.id Nothing mbRide
    Just driver -> buildBookingCancellationReason booking.id (Just driver.id) mbRide
  Esq.runTransaction $ do
    QRB.updateStatus booking.id DRB.CANCELLED
    QBCR.upsert bookingCancellationReason
    whenJust mbRide $ \ride -> do
      QRide.updateStatus ride.id DRide.CANCELLED
      driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      QDFS.updateStatus ride.driverId $ DMode.getDriverStatus driverInfo.mode driverInfo.active
  whenJust mbRide $ \ride -> do
    SRide.clearCache ride.driverId
    DLoc.updateOnRide (cast ride.driverId) False
  fork "cancelBooking - Notify BAP" $ do
    BP.sendBookingCancelledUpdateToBAP BP.SIMPLE booking transporter bookingCancellationReason.source
  whenJust mbRide $ \ride ->
    case mbDriver of
      Nothing -> throwError (PersonNotFound ride.driverId.getId)
      Just driver -> do
        fork "cancelRide - Notify driver" $ do
          Notify.notifyOnCancel transporter.id booking driver.id driver.deviceToken bookingCancellationReason.source
  where
    buildBookingCancellationReason bookingId driverId ride = do
      return $
        DBCR.BookingCancellationReason
          { driverId = driverId,
            bookingId,
            rideId = (.id) <$> ride,
            source = DBCR.ByApplication,
            reasonCode = Nothing,
            additionalInfo = Nothing
          }
