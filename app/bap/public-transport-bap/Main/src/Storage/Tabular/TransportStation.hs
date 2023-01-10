{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.TransportStation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.TransportStation as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TransportStationT sql=transport_station
      id Text
      name Text
      stationCode Text
      lat Double
      lon Double
      Primary id
      deriving Generic
    |]

instance TEntityKey TransportStationT where
  type DomainKey TransportStationT = Id Domain.TransportStation
  fromKey (TransportStationTKey _id) = Id _id
  toKey id = TransportStationTKey id.getId

instance TType TransportStationT Domain.TransportStation where
  fromTType TransportStationT {..} = do
    return $
      Domain.TransportStation
        { id = Id id,
          ..
        }
  toTType Domain.TransportStation {..} =
    TransportStationT
      { id = id.getId,
        ..
      }