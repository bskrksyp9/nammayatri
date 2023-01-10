{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Booking.Table where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common (HighPrecMoney)
import Beckn.Types.Id
import qualified Domain.Types.Booking.Type as Domain
import qualified Domain.Types.FarePolicy.FareProduct as Domain
import qualified Domain.Types.Vehicle as Veh
import Storage.Tabular.Booking.BookingLocation (BookingLocationTId)
import Storage.Tabular.FarePolicy.FareProduct ()
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.RiderDetails (RiderDetailsTId)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.BookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BookingT sql=booking
      id Text
      fareProductType Domain.FareProductType
      status Domain.BookingStatus
      providerId MerchantTId
      bapId Text
      bapUri Text
      startTime UTCTime
      riderId RiderDetailsTId Maybe
      fromLocationId BookingLocationTId
      vehicleVariant Veh.Variant
      estimatedFare HighPrecMoney
      discount HighPrecMoney Maybe
      estimatedTotalFare HighPrecMoney
      reallocationsCount Int
      riderName Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey BookingT where
  type DomainKey BookingT = Id Domain.Booking
  fromKey (BookingTKey _id) = Id _id
  toKey (Id id) = BookingTKey id