{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FullEntityBuilders where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Booking.Type as Booking
import qualified Domain.Types.FarePolicy.FareProduct as Domain
import Domain.Types.FarePolicy.OneWayFarePolicy
import Domain.Types.Quote as Quote
import qualified Storage.Queries.FarePolicy.Discount as QDisc
import qualified Storage.Queries.FarePolicy.OneWayFarePolicy.PerExtraKmRate as QExtraKmRate
import qualified Storage.Queries.Quote.OneWayQuote as QOneWayQuote
import qualified Storage.Queries.Quote.RentalQuote as QRentalQuote
import Storage.Tabular.Booking as Booking
import Storage.Tabular.Booking.BookingLocation (BookingLocationT)
import Storage.Tabular.Booking.RentalBooking (RentalBookingT)
import Storage.Tabular.FarePolicy.OneWayFarePolicy
import Storage.Tabular.Quote as Quote
import Storage.Tabular.Quote.RentalQuote (RentalQuoteT (..))

buildFullOneWayFarePolicy :: Transactionable m => OneWayFarePolicyT -> DTypeBuilder m (SolidType FullOneWayFarePolicyT)
buildFullOneWayFarePolicy farePolicy = do
  let orgId = fromKey $ Storage.Tabular.FarePolicy.OneWayFarePolicy.organizationId farePolicy
      vehicleVariant = Storage.Tabular.FarePolicy.OneWayFarePolicy.vehicleVariant farePolicy
  perExtraKmRate <- QExtraKmRate.findAll' orgId vehicleVariant
  discount <- QDisc.findAll' orgId vehicleVariant

  return $ extractSolidType @OneWayFarePolicy (farePolicy, perExtraKmRate, discount)

buildFullQuote :: Transactionable m => QuoteT -> DTypeBuilder m (Maybe (SolidType FullQuoteT))
buildFullQuote quoteT@QuoteT {..} = runMaybeT $ do
  quoteDetails <- case fareProductType of
    Domain.RENTAL -> do
      rentalQuoteT@RentalQuoteT {..} <- MaybeT $ QRentalQuote.findByQuoteId' (Id id)
      rentalFarePolicyT <- MaybeT . Esq.findById' $ fromKey rentalFarePolicyId
      return $ Quote.RentalDetailsT (rentalQuoteT, rentalFarePolicyT)
    Domain.ONE_WAY -> do
      oneWayQuoteT <- MaybeT $ QOneWayQuote.findByQuoteId' (Id id)
      return $ Quote.OneWayDetailsT oneWayQuoteT
  return $ extractSolidType @Quote (quoteT, quoteDetails)

buildFullBooking ::
  Transactionable m =>
  (BookingT, BookingLocationT, Maybe BookingLocationT, Maybe RentalBookingT) ->
  DTypeBuilder m (Maybe (SolidType FullBookingT))
buildFullBooking (bookingT@BookingT {..}, fromLocT, mbToLocT, mbRentalBookingT) = runMaybeT $ do
  bookingDetailsT <- case toLocationId of
    Nothing -> MaybeT $ pure (Booking.RentalDetailsT <$> mbRentalBookingT)
    Just _ -> MaybeT $ pure (Booking.OneWayDetailsT <$> mbToLocT)
  return $ extractSolidType @Booking (bookingT, fromLocT, bookingDetailsT)