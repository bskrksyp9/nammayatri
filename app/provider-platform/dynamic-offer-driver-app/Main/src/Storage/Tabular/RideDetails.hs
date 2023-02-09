{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RideDetails where

import qualified Domain.Types.Ride as SR
import qualified Domain.Types.RideDetails as Domain
import qualified Domain.Types.Vehicle as SV
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Ride (RideTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideDetailsT sql=ride_details
      id RideTId
      driverName Text
      driverNumberEncrypted Text Maybe
      driverNumberHash DbHash Maybe
      driverCountryCode Text Maybe
      vehicleNumber Text
      vehicleColor Text Maybe
      vehicleVariant SV.Variant Maybe
      vehicleModel Text Maybe
      vehicleClass Text Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey RideDetailsT where
  type DomainKey RideDetailsT = Id SR.Ride
  fromKey (RideDetailsTKey _id) = fromKey _id
  toKey id = RideDetailsTKey $ toKey id

instance TType RideDetailsT Domain.RideDetails where
  fromTType RideDetailsT {..} = do
    return $
      Domain.RideDetails
        { id = fromKey id,
          driverNumber = EncryptedHashed <$> (Encrypted <$> driverNumberEncrypted) <*> driverNumberHash,
          ..
        }
  toTType Domain.RideDetails {..} =
    RideDetailsT
      { id = toKey id,
        driverNumberEncrypted = driverNumber <&> unEncrypted . (.encrypted),
        driverNumberHash = driverNumber <&> (.hash),
        ..
      }