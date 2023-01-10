module Domain.Action.UI.BookingList where

import Beckn.Prelude
import Beckn.Storage.Esqueleto (runInReplica)
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Booking.API
import Domain.Types.Booking.Type
import Storage.Queries.Booking as QBooking
import Storage.Queries.PaymentTransaction as QPT
import qualified Storage.Queries.TransportStation as QTransportStation
import Tools.Auth
import Tools.Error

bookingListHandler :: EsqDBReplicaFlow m r => PersonId -> Maybe Integer -> Maybe Integer -> Maybe BookingStatus -> m BookingListRes
bookingListHandler personId mbLimit mbOffset mbBookingStatus = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  logDebug $ getId personId
  bList <- runInReplica $ QBooking.findAllByRequestorId personId limit offset mbBookingStatus
  logDebug $ show bList
  BookingListRes
    <$> traverse buildBookingListRes bList

buildBookingListRes :: EsqDBReplicaFlow m r => Booking -> m BookingAPIEntity
buildBookingListRes booking = do
  departureStation <- runInReplica $ QTransportStation.findById booking.departureStationId >>= fromMaybeM TransportStationNotFound
  arrivalStation <- runInReplica $ QTransportStation.findById booking.arrivalStationId >>= fromMaybeM TransportStationNotFound
  paymentTrans <- runInReplica $ QPT.findByBookingId booking.id
  return $ makeBookingAPIEntity booking departureStation arrivalStation paymentTrans