module Domain.Endpoints.UI.BookingList where

import Beckn.Prelude
import Beckn.Utils.Common
import Domain.Types.Booking.API
import Domain.Types.Booking.Type
import Storage.Queries.Booking as QBooking
import Storage.Queries.PaymentTransaction as QPT
import qualified Storage.Queries.TransportStation as QTransportStation
import Tools.Auth
import Tools.Error

bookingListHandler :: EsqDBFlow m r => PersonId -> Maybe Integer -> Maybe Integer -> m BookingListRes
bookingListHandler personId mbLimit mbOffset = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  bList <- QBooking.findAllByRequestorId personId limit offset
  BookingListRes
    <$> traverse buildBookingListRes bList

buildBookingListRes :: EsqDBFlow m r => Booking -> m BookingAPIEntity
buildBookingListRes booking = do
  departureStation <- QTransportStation.findById booking.departureStationId >>= fromMaybeM TransportStationNotFound
  arrivalStation <- QTransportStation.findById booking.arrivalStationId >>= fromMaybeM TransportStationNotFound
  paymentTrans <- QPT.findByBookingId booking.id
  return $ makeBookingAPIEntity booking departureStation arrivalStation paymentTrans