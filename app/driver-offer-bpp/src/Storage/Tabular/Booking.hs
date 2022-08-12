{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Booking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.Vehicle.Variant as Veh
import Storage.Tabular.Booking.BookingLocation hiding (createdAt, id, updatedAt)
import Storage.Tabular.DriverQuote (DriverQuoteTId)
import qualified Storage.Tabular.FareParameters as Fare
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.RiderDetails (RiderDetailsTId)
import Storage.Tabular.Vehicle ()
import Types.Money (RoundedMoney)

derivePersistField "Domain.BookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BookingT sql=booking
      id Text
      quoteId DriverQuoteTId
      status Domain.BookingStatus
      providerId OrganizationTId
      bapId Text
      bapUri Text
      startTime UTCTime
      riderId RiderDetailsTId Maybe
      fromLocationId BookingLocationTId
      toLocationId BookingLocationTId
      vehicleVariant Veh.Variant
      estimatedDistance Double
      createdAt UTCTime
      updatedAt UTCTime
      estimatedFare RoundedMoney
      fareParametersId Fare.FareParametersTId

      Primary id
      deriving Generic
    |]

instance TEntityKey BookingT where
  type DomainKey BookingT = Id Domain.Booking
  fromKey (BookingTKey _id) = Id _id
  toKey (Id id) = BookingTKey id

instance TType (BookingT, BookingLocationT, BookingLocationT, Fare.FareParametersT) Domain.Booking where
  fromTType (BookingT {..}, fromLoc, toLoc, fareParametersT) = do
    pUrl <- parseBaseUrl bapUri
    let fromLoc_ = mkDomainBookingLocation fromLoc
        toLoc_ = mkDomainBookingLocation toLoc
    return $
      Domain.Booking
        { id = Id id,
          quoteId = fromKey quoteId,
          providerId = fromKey providerId,
          fromLocation = fromLoc_,
          toLocation = toLoc_,
          bapUri = pUrl,
          riderId = fromKey <$> riderId,
          estimatedDistance = HighPrecMeters estimatedDistance,
          fareParams = Fare.mkDomainFromTabularFareParams fareParametersT,
          ..
        }
  toTType Domain.Booking {..} =
    let fareParamsId = cast id
     in ( BookingT
            { id = getId id,
              quoteId = toKey quoteId,
              providerId = toKey providerId,
              fromLocationId = toKey fromLocation.id,
              toLocationId = toKey toLocation.id,
              bapUri = showBaseUrl bapUri,
              riderId = toKey <$> riderId,
              estimatedDistance = getHighPrecMeters estimatedDistance,
              fareParametersId = toKey fareParamsId,
              ..
            },
          mkTabularBookingLocation fromLocation,
          mkTabularBookingLocation toLocation,
          Fare.mkTabularFromDomainFareParams fareParamsId fareParams
        )
