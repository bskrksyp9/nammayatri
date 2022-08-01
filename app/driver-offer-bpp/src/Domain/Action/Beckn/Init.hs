module Domain.Action.Beckn.Init where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Error.Throwing
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.SearchRequest as QSR

-- fields that are not used because of stateful init API
--    vehicleVariant :: Veh.Variant,
--    fromLocation :: LatLong,
--    toLocation :: LatLong,
--    startTime :: UTCTime,

data InitReq = InitReq
  { driverQuoteId :: Id DQuote.DriverQuote,
    bapId :: Text,
    bapUri :: BaseUrl
  }

data InitRes = InitRes
  { booking :: DRB.Booking,
    transporter :: DOrg.Organization
  }

-- FIXME: if the init request comes twice, this causes error
-- because of the unique keys violating
mkBookingLocation :: DLoc.SearchReqLocation -> DLoc.BookingLocation
mkBookingLocation DLoc.SearchReqLocation {..} = do
  let address = DLoc.LocationAddress {..}
  DLoc.BookingLocation
    { id = cast id,
      ..
    }

handler :: (EsqDBFlow m r) => Id DOrg.Organization -> InitReq -> m InitRes
handler orgId req = do
  transporter <- QOrg.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId)
  driverQuote <- QDQuote.findById req.driverQuoteId >>= fromMaybeM (QuoteNotFound req.driverQuoteId.getId)
  searchRequest <- QSR.findById driverQuote.searchRequestId >>= fromMaybeM (SearchRequestNotFound driverQuote.searchRequestId.getId)
  booking <- buildBooking searchRequest driverQuote
  Esq.runTransaction $
    QRB.create booking
  pure InitRes {..}
  where
    buildBooking searchRequest driverQuote = do
      id <- Id <$> generateGUID
      now <- getCurrentTime
      pure
        DRB.Booking
          { quoteId = req.driverQuoteId,
            status = DRB.NEW,
            providerId = orgId,
            bapId = req.bapId,
            bapUri = req.bapUri,
            startTime = searchRequest.startTime,
            riderId = Nothing,
            vehicleVariant = driverQuote.vehicleVariant,
            estimatedDistance = HighPrecMeters $ driverQuote.distance,
            createdAt = now,
            updatedAt = now,
            fromLocation = mkBookingLocation searchRequest.fromLocation,
            toLocation = mkBookingLocation searchRequest.toLocation,
            fareParams = driverQuote.fareParams,
            ..
          }