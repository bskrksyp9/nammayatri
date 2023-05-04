module SharedLogic.Confirm where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.TripLocation as DBL
import qualified Domain.Types.DriverOffer as DDriverOffer
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Quote as DQuote
import Domain.Types.RentalSlab
import qualified Domain.Types.SearchRequest as DSReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSRLoc
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Randomizer (getRandomElement)
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSReq
import Tools.Error

data DConfirmRes = DConfirmRes
  { providerId :: Text,
    providerUrl :: BaseUrl,
    fromLoc :: LatLong,
    toLoc :: Maybe LatLong,
    vehicleVariant :: VehicleVariant,
    quoteDetails :: ConfirmQuoteDetails,
    startTime :: UTCTime,
    booking :: DRB.Booking,
    searchRequestId :: Id DSReq.SearchRequest,
    city :: Text,
    maxEstimatedDistance :: Maybe HighPrecMeters
  }
  deriving (Show, Generic)

data ConfirmQuoteDetails
  = ConfirmOneWayDetails
  | ConfirmRentalDetails RentalSlabAPIEntity
  | ConfirmAutoDetails (Id DDriverOffer.BPPQuote)
  | ConfirmOneWaySpecialZoneDetails Text
  deriving (Show, Generic)

confirm :: (EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Id DQuote.Quote -> m DConfirmRes
confirm personId quoteId = do
  quote <- QQuote.findById quoteId >>= fromMaybeM (QuoteDoesNotExist quoteId.getId)
  now <- getCurrentTime
  case quote.quoteDetails of
    DQuote.OneWayDetails _ -> pure ()
    DQuote.RentalDetails _ -> pure ()
    DQuote.DriverOfferDetails driverOffer -> do
      estimate <- QEstimate.findById driverOffer.estimateId >>= fromMaybeM EstimateNotFound
      when (DEstimate.isCancelled estimate.status) $ throwError $ EstimateCancelled estimate.id.getId
      when (driverOffer.validTill < now) $
        throwError $ QuoteExpired quote.id.getId
    DQuote.OneWaySpecialZoneDetails _ -> pure ()
  searchRequest <- QSReq.findById quote.requestId >>= fromMaybeM (SearchRequestNotFound quote.requestId.getId)
  activeBooking <- QRideB.findByRiderIdAndStatus personId DRB.activeBookingStatus
  unless (null activeBooking) $ throwError $ InvalidRequest "ACTIVE_BOOKING_PRESENT"
  when (searchRequest.validTill < now) $
    throwError SearchRequestExpired
  unless (searchRequest.riderId == personId) $ throwError AccessDenied
  let fromLocation = searchRequest.fromLocation
      mbToLocation = searchRequest.toLocation
  bFromLocation <- buildTripLocation now fromLocation
  mbBToLocation <- traverse (buildTripLocation now) mbToLocation
  exophone <- findRandomExophone searchRequest.merchantId
  booking <- buildBooking searchRequest quote bFromLocation mbBToLocation exophone now Nothing
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  let details = mkConfirmQuoteDetails quote.quoteDetails
  DB.runTransaction $ do
    QRideB.create booking
    QPFS.updateStatus searchRequest.riderId DPFS.WAITING_FOR_DRIVER_ASSIGNMENT {bookingId = booking.id, validTill = searchRequest.validTill}
    QEstimate.updateStatusbyRequestId quote.requestId DEstimate.COMPLETED
  return $
    DConfirmRes
      { booking,
        providerId = quote.providerId,
        providerUrl = quote.providerUrl,
        fromLoc = LatLong {lat = fromLocation.lat, lon = fromLocation.lon},
        toLoc = mbToLocation <&> \toLocation -> LatLong {lat = toLocation.lat, lon = toLocation.lon},
        vehicleVariant = quote.vehicleVariant,
        quoteDetails = details,
        startTime = searchRequest.startTime,
        searchRequestId = searchRequest.id,
        city = merchant.city,
        maxEstimatedDistance = searchRequest.maxDistance
      }
  where
    mkConfirmQuoteDetails :: DQuote.QuoteDetails -> ConfirmQuoteDetails
    mkConfirmQuoteDetails = \case
      DQuote.OneWayDetails _ -> ConfirmOneWayDetails
      DQuote.RentalDetails RentalSlab {..} -> ConfirmRentalDetails $ RentalSlabAPIEntity {..}
      DQuote.DriverOfferDetails driverOffer -> ConfirmAutoDetails driverOffer.bppQuoteId
      DQuote.OneWaySpecialZoneDetails details -> ConfirmOneWaySpecialZoneDetails details.quoteId

buildBooking ::
  MonadFlow m =>
  DSReq.SearchRequest ->
  DQuote.Quote ->
  DBL.TripLocation ->
  Maybe DBL.TripLocation ->
  DExophone.Exophone ->
  UTCTime ->
  Maybe Text ->
  m DRB.Booking
buildBooking searchRequest quote fromLoc mbToLoc exophone now otpCode = do
  id <- generateGUID
  bookingDetails <- buildBookingDetails
  return $
    DRB.Booking
      { id = Id id,
        transactionId = searchRequest.id.getId,
        bppBookingId = Nothing,
        status = DRB.NEW,
        providerId = quote.providerId,
        primaryExophone = exophone.primaryPhone,
        quoteId = Just $ quote.id,
        providerUrl = quote.providerUrl,
        providerName = quote.providerName,
        providerMobileNumber = quote.providerMobileNumber,
        startTime = searchRequest.startTime,
        riderId = searchRequest.riderId,
        fromLocation = fromLoc,
        estimatedFare = quote.estimatedFare,
        discount = quote.discount,
        estimatedTotalFare = quote.estimatedTotalFare,
        vehicleVariant = quote.vehicleVariant,
        bookingDetails,
        tripTerms = quote.tripTerms,
        merchantId = searchRequest.merchantId,
        createdAt = now,
        updatedAt = now
      }
  where
    buildBookingDetails = case quote.quoteDetails of
      DQuote.OneWayDetails _ -> DRB.OneWayDetails <$> buildOneWayDetails
      DQuote.RentalDetails rentalSlab -> pure $ DRB.RentalDetails rentalSlab
      DQuote.DriverOfferDetails _ -> DRB.DriverOfferDetails <$> buildOneWayDetails
      DQuote.OneWaySpecialZoneDetails _ -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails
    buildOneWayDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWayBookingDetails {..}
    buildOneWaySpecialZoneDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWaySpecialZoneBookingDetails {..}

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m DExophone.Exophone
findRandomExophone merchantId = do
  exophones <- CQExophone.findAllByMerchantId merchantId
  nonEmptyExophones <- case exophones of
    [] -> throwError $ ExophoneNotFound merchantId.getId
    e : es -> pure $ e :| es
  getRandomElement nonEmptyExophones

buildTripLocation :: MonadGuid m => UTCTime -> DSRLoc.SearchReqLocation -> m DBL.TripLocation
buildTripLocation now DSRLoc.SearchReqLocation {..} = do
  locId <- generateGUID
  return
    DBL.TripLocation
      { id = locId,
        lat,
        lon,
        address,
        createdAt = now,
        updatedAt = now
      }
