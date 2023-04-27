{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Booking where

import Domain.Types.Booking
import qualified Domain.Types.Booking as Booking
import Domain.Types.Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import Domain.Types.RiderDetails (RiderDetails)
import qualified Domain.Types.SearchRequest as DSR
import Kernel.External.Maps.Types as MT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (findById, isNothing)
import Kernel.Types.Id
import Kernel.Types.Time
import qualified Storage.Queries.DriverQuote as QDQuote
import Storage.Tabular.Booking
import Storage.Tabular.Booking.BookingLocation
import Storage.Tabular.DriverQuote as DriverQuote
import qualified Storage.Tabular.FareParameters as Fare
import Storage.Tabular.Ride

-- fareParams already created with driverQuote
create :: Booking -> SqlDB ()
create dBooking = Esq.runTransaction $
  withFullEntity dBooking $ \(booking, fromLoc, toLoc, _fareParams) -> do
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' booking

baseBookingTable ::
  From
    ( Table BookingT
        :& Table BookingLocationT
        :& Table BookingLocationT
        :& Table Fare.FareParametersT
    )
baseBookingTable =
  table @BookingT
    `innerJoin` table @BookingLocationT `Esq.on` (\(rb :& loc1) -> rb ^. BookingFromLocationId ==. loc1 ^. BookingLocationTId)
    `innerJoin` table @BookingLocationT `Esq.on` (\(rb :& _ :& loc2) -> rb ^. BookingToLocationId ==. loc2 ^. BookingLocationTId)
    `innerJoin` table @Fare.FareParametersT
      `Esq.on` ( \(rb :& _ :& _ :& farePars) ->
                   rb ^. BookingFareParametersId ==. farePars ^. Fare.FareParametersTId
               )

findById :: Transactionable m => Id Booking -> m (Maybe Booking)
findById bookingId = buildDType $
  fmap (fmap $ extractSolidType @Booking) $
    Esq.findOne' $ do
      (rb :& bFromLoc :& bToLoc :& farePars) <-
        from baseBookingTable
      where_ $ rb ^. BookingTId ==. val (toKey bookingId)
      pure (rb, bFromLoc, bToLoc, farePars)

findBySearchReq :: (Transactionable m) => Id DSR.SearchRequest -> m (Maybe Booking)
findBySearchReq searchReqId = buildDType $ do
  mbDriverQuoteT <- QDQuote.findDriverQuoteBySearchId searchReqId
  let mbDriverQuoteId = Id . DriverQuote.id <$> mbDriverQuoteT
  mbBookingT <- (join <$>) $ mapM (findBookingByDriverQuoteId . getId) mbDriverQuoteId

  join <$> mapM buildFullBooking mbBookingT

findBookingByDriverQuoteId :: Transactionable m => Text -> DTypeBuilder m (Maybe BookingT)
findBookingByDriverQuoteId driverQuoteId = Esq.findOne' $ do
  booking <- from $ table @BookingT
  where_ $ booking ^. BookingQuoteId ==. val driverQuoteId
  pure booking

buildFullBooking ::
  Transactionable m =>
  BookingT ->
  DTypeBuilder m (Maybe (SolidType FullBookingT))
buildFullBooking bookingT@BookingT {..} = runMaybeT $ do
  fromLocationT <- MaybeT $ Esq.findById' @BookingLocationT (fromKey fromLocationId)
  toLocationT <- MaybeT $ Esq.findById' @BookingLocationT (fromKey toLocationId)
  fareParamsT <- MaybeT $ Esq.findById' @Fare.FareParametersT (fromKey fareParametersId)
  return $ extractSolidType @Booking (bookingT, fromLocationT, toLocationT, fareParamsT)

updateStatus :: Id Booking -> BookingStatus -> SqlDB ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val rbStatus,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey rbId)

updateRiderId :: Id Booking -> Id RiderDetails -> SqlDB ()
updateRiderId rbId riderId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingRiderId =. val (Just $ toKey riderId),
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey rbId)

updateRiderName :: Id Booking -> Text -> SqlDB ()
updateRiderName bookingId riderName = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingRiderName =. val (Just riderName),
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey bookingId)

updateSpecialZoneOtpCode :: Id Booking -> Text -> SqlDB ()
updateSpecialZoneOtpCode bookingId specialZoneOtpCode = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingSpecialZoneOtpCode =. val (Just specialZoneOtpCode),
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey bookingId)

findStuckBookings :: Transactionable m => Id Merchant -> [Id Booking] -> UTCTime -> m [Id Booking]
findStuckBookings merchantId bookingIds now = do
  Esq.findAll $ do
    booking <- from $ table @BookingT
    let upcoming6HrsCond =
          booking ^. BookingCreatedAt +. Esq.interval [Esq.HOUR 6] <=. val now
    where_ $
      booking ^. BookingProviderId ==. val (toKey merchantId)
        &&. booking ^. BookingTId `in_` valList (toKey <$> bookingIds)
        &&. (booking ^. BookingStatus ==. val NEW &&. upcoming6HrsCond)
    pure $ booking ^. BookingTId

findBookingBySpecialZoneOTP :: Transactionable m => Id Merchant -> Text -> UTCTime -> m (Maybe Booking)
findBookingBySpecialZoneOTP merchantId otpCode now = do
  bookingId <- findBookingIdBySpecialZoneOTP merchantId otpCode now
  maybe
    (return Nothing)
    findById
    bookingId

findBookingIdBySpecialZoneOTP :: Transactionable m => Id Merchant -> Text -> UTCTime -> m (Maybe (Id Booking))
findBookingIdBySpecialZoneOTP merchantId otpCode now = do
  Esq.findOne $ do
    booking <- from $ table @BookingT
    let otpExpiryCondition =
          booking ^. BookingCreatedAt +. Esq.interval [Esq.MINUTE 30] >=. val now
    where_ $
      booking ^. BookingSpecialZoneOtpCode ==. val (Just otpCode)
        &&. (booking ^. BookingStatus ==. val NEW &&. otpExpiryCondition)
        &&. booking ^. BookingProviderId ==. val (toKey merchantId)
    pure $ booking ^. BookingTId

cancelBookings :: [Id Booking] -> UTCTime -> SqlDB ()
cancelBookings bookingIds now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val CANCELLED,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId `in_` valList (toKey <$> bookingIds)

findByDriverIdTripEndLatLonIfRideStatusInProgress :: (Transactionable m, MonadTime m) => Id Person.Person -> m (Maybe MT.LatLong)
findByDriverIdTripEndLatLonIfRideStatusInProgress driverId = do
  res <- Esq.findOne $ do
    (_ :& _ :& _ :& bookingLocationInfo) <-
      from $
        table @RideT
          `innerJoin` table @BookingT
          `Esq.on` ( \(rideInfo :& bookingInfo) ->
                       rideInfo ^. RideBookingId ==. bookingInfo ^. BookingTId
                         &&. rideInfo ^. RideDriverId ==. val (toKey driverId)
                         &&. rideInfo ^. RideStatus ==. val Ride.INPROGRESS
                   )
          `innerJoin` table @DriverQuoteT
          `Esq.on` ( \(_ :& bookingInfo :& driverQuoteInfo) ->
                       driverQuoteInfo ^. DriverQuoteId ==. bookingInfo ^. BookingQuoteId
                         &&. bookingInfo ^. BookingStatus ==. val Booking.TRIP_ASSIGNED
                   )
          `innerJoin` table @BookingLocationT
          `Esq.on` ( \(_ :& bookingInfo :& _ :& bookingLocationInfo) ->
                       bookingInfo ^. BookingToLocationId ==. bookingLocationInfo ^. BookingLocationTId
                   )
    pure (bookingLocationInfo ^. BookingLocationLat, bookingLocationInfo ^. BookingLocationLon)
  pure $ uncurry MT.LatLong <$> res
