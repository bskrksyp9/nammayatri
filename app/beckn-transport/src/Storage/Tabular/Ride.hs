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
import Beckn.Types.Id
import qualified Domain.Types.Ride as Domain
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.RideBooking (RideBookingTId)
import Storage.Tabular.Vehicle (VehicleTId)

derivePersistField "Domain.RideStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideT sql=ride
      id Text
      bookingId RideBookingTId
      shortId Text
      status Domain.RideStatus
      driverId PersonTId
      vehicleId VehicleTId
      otp Text
      trackingUrl Text
      fare Amount Maybe
      totalFare Amount Maybe
      traveledDistance Double
      chargeableDistance Double Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RideT where
  type DomainKey RideT = Id Domain.Ride
  fromKey (RideTKey _id) = Id _id
  toKey (Id id) = RideTKey id

instance TEntity RideT Domain.Ride where
  fromTEntity entity = do
    let RideT {..} = entityVal entity
    return $
      Domain.Ride
        { id = Id id,
          bookingId = fromKey bookingId,
          shortId = ShortId shortId,
          driverId = fromKey driverId,
          vehicleId = fromKey vehicleId,
          ..
        }
  toTType Domain.Ride {..} =
    RideT
      { id = getId id,
        bookingId = toKey bookingId,
        shortId = getShortId shortId,
        driverId = toKey driverId,
        vehicleId = toKey vehicleId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a