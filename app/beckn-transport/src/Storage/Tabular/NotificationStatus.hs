{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.NotificationStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.NotificationStatus as Domain
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.RideBooking (RideBookingTId)

derivePersistField "Domain.AnswerStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    NotificationStatusT sql=notification_status
      id Text
      rideBookingId RideBookingTId
      driverId PersonTId
      status Domain.AnswerStatus
      expiresAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey NotificationStatusT where
  type DomainKey NotificationStatusT = Id Domain.NotificationStatus
  fromKey (NotificationStatusTKey _id) = Id _id
  toKey (Id id) = NotificationStatusTKey id

instance TEntity NotificationStatusT Domain.NotificationStatus where
  fromTEntity entity = do
    let NotificationStatusT {..} = entityVal entity
    return $
      Domain.NotificationStatus
        { id = Id id,
          driverId = cast $ fromKey driverId,
          rideBookingId = fromKey rideBookingId,
          ..
        }
  toTType Domain.NotificationStatus {..} =
    NotificationStatusT
      { id = getId id,
        driverId = toKey $ cast driverId,
        rideBookingId = toKey rideBookingId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a