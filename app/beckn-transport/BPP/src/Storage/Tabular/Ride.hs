{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Ride where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount (Amount)
import Beckn.Types.Common (HighPrecMeters (..))
import Beckn.Types.Id
import qualified Domain.Types.Ride as Domain
import Storage.Tabular.Booking (BookingTId)
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.RideStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideT sql=ride
      id Text
      bookingId BookingTId
      shortId Text
      status Domain.RideStatus
      driverId PersonTId
      otp Text
      trackingUrl Text
      fare Amount Maybe
      totalFare Amount Maybe
      traveledDistance Double
      chargeableDistance Double Maybe
      tripStartTime UTCTime Maybe
      tripEndTime UTCTime Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RideT where
  type DomainKey RideT = Id Domain.Ride
  fromKey (RideTKey _id) = Id _id
  toKey (Id id) = RideTKey id

instance TType RideT Domain.Ride where
  fromTType RideT {..} = do
    tUrl <- parseBaseUrl trackingUrl
    return $
      Domain.Ride
        { id = Id id,
          bookingId = fromKey bookingId,
          shortId = ShortId shortId,
          driverId = fromKey driverId,
          traveledDistance = HighPrecMeters traveledDistance,
          chargeableDistance = HighPrecMeters <$> chargeableDistance,
          trackingUrl = tUrl,
          ..
        }
  toTType Domain.Ride {..} =
    RideT
      { id = getId id,
        bookingId = toKey bookingId,
        shortId = getShortId shortId,
        driverId = toKey driverId,
        traveledDistance = getHighPrecMeters traveledDistance,
        chargeableDistance = getHighPrecMeters <$> chargeableDistance,
        trackingUrl = showBaseUrl trackingUrl,
        ..
      }