{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverInformation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.DriverInformation as Domain
import Domain.Types.Person (Person)
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverInformationT sql=driver_information
      driverId PersonTId
      adminId PersonTId Maybe
      active Bool
      onRide Bool
      enabled Bool
      blocked Bool
      optForRental Bool
      createdAt UTCTime
      updatedAt UTCTime
      canDowngradeToSedan Bool
      canDowngradeToHatchback Bool
      Primary driverId
      deriving Generic
    |]

instance TEntityKey DriverInformationT where
  type DomainKey DriverInformationT = Id Person
  fromKey (DriverInformationTKey _id) = fromKey _id
  toKey id = DriverInformationTKey $ toKey id

instance TType DriverInformationT Domain.DriverInformation where
  fromTType DriverInformationT {..} = do
    return $
      Domain.DriverInformation
        { driverId = fromKey driverId,
          adminId = fromKey <$> adminId,
          ..
        }
  toTType Domain.DriverInformation {..} =
    DriverInformationT
      { driverId = toKey driverId,
        adminId = toKey <$> adminId,
        ..
      }