{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.Instances where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.FarePolicy.RentalFarePolicy as Domain
import qualified Domain.Types.Quote as Domain
import Storage.Tabular.FarePolicy.FareProduct ()
import Storage.Tabular.FarePolicy.RentalFarePolicy (RentalFarePolicyT)
import Storage.Tabular.Quote.OneWayQuote
import Storage.Tabular.Quote.RentalQuote
import Storage.Tabular.Quote.Table
import Storage.Tabular.Vehicle ()

data QuoteDetailsT = OneWayDetailsT OneWayQuoteT | RentalDetailsT (RentalQuoteT, RentalFarePolicyT)

type FullQuoteT = (QuoteT, QuoteDetailsT)

instance TType FullQuoteT Domain.Quote where
  fromTType (QuoteT {..}, tQuoteDetails) = do
    quoteDetails <- case tQuoteDetails of
      RentalDetailsT (_, rental) -> do
        Domain.RentalDetails <$> fromTType rental
      OneWayDetailsT oneWay -> do
        let oneWayQuoteEntity = oneWayQuoteFromTType oneWay
        return . Domain.OneWayDetails $
          Domain.OneWayQuoteDetails
            { distance = oneWayQuoteEntity.distance,
              distanceToNearestDriver = oneWayQuoteEntity.distance,
              estimatedFinishTime = oneWayQuoteEntity.estimatedFinishTime
            }
    pure
      Domain.Quote
        { id = Id id,
          requestId = fromKey requestId,
          providerId = fromKey providerId,
          estimatedFare = roundToIntegral estimatedFare,
          discount = roundToIntegral <$> discount,
          estimatedTotalFare = roundToIntegral estimatedTotalFare,
          ..
        }

  toTType Domain.Quote {..} = do
    let details = case quoteDetails of
          Domain.OneWayDetails oneWay -> OneWayDetailsT $ oneWayQuoteToTType id oneWay
          Domain.RentalDetails rental -> do
            let rentalQuoteT = rentalToTType id rental.id
                rentalFarePolicyT = toTType rental
            RentalDetailsT (rentalQuoteT, rentalFarePolicyT)
    let quoteT =
          QuoteT
            { id = getId id,
              fareProductType = Domain.getFareProductType quoteDetails,
              requestId = toKey requestId,
              providerId = toKey providerId,
              estimatedFare = fromIntegral estimatedFare,
              discount = fromIntegral <$> discount,
              estimatedTotalFare = fromIntegral estimatedTotalFare,
              ..
            }
    (quoteT, details)

oneWayQuoteFromTType :: OneWayQuoteT -> Domain.OneWayQuoteDetails
oneWayQuoteFromTType OneWayQuoteT {..} = do
  Domain.OneWayQuoteDetails
    { distance = roundToIntegral distance,
      distanceToNearestDriver = roundToIntegral distanceToNearestDriver,
      ..
    }

oneWayQuoteToTType :: Id Domain.Quote -> Domain.OneWayQuoteDetails -> OneWayQuoteT
oneWayQuoteToTType quoteId Domain.OneWayQuoteDetails {..} =
  OneWayQuoteT
    { quoteId = toKey quoteId,
      distance = fromIntegral distance,
      distanceToNearestDriver = fromIntegral distanceToNearestDriver,
      ..
    }

rentalToTType :: Id Domain.Quote -> Id Domain.RentalFarePolicy -> RentalQuoteT
rentalToTType quoteId rentalFarePolicyId =
  RentalQuoteT
    { quoteId = toKey quoteId,
      rentalFarePolicyId = toKey rentalFarePolicyId
    }