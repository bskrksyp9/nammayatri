{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy.PerExtraKmRate where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.FarePolicy.PerExtraKmRate as Domain
import Domain.Types.Organization (Organization)
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PerExtraKmRateT sql=fare_policy_per_extra_km_rate
      id Text
      organizationId OrganizationTId
      distanceRangeStart Double
      fare Double
      Primary id
      deriving Generic
    |]

instance TEntityKey PerExtraKmRateT where
  type DomainKey PerExtraKmRateT = Id Domain.PerExtraKmRate
  fromKey (PerExtraKmRateTKey _id) = Id _id
  toKey (Id id) = PerExtraKmRateTKey id

type FullPerExtraKmRate = (Id Domain.PerExtraKmRate, Id Organization, Domain.PerExtraKmRate)

instance TEntity PerExtraKmRateT FullPerExtraKmRate where
  fromTEntity entity = do
    let PerExtraKmRateT {..} = entityVal entity
    return
      ( Id id,
        fromKey organizationId,
        Domain.PerExtraKmRate
          { distanceRangeStart = toRational distanceRangeStart,
            fare = toRational fare,
            ..
          }
      )
  toTType (id, orgId, Domain.PerExtraKmRate {..}) =
    PerExtraKmRateT
      { id = getId id,
        organizationId = toKey orgId,
        distanceRangeStart = fromRational distanceRangeStart,
        fare = fromRational fare,
        ..
      }
  toTEntity a@(id, _, _) =
    Entity (toKey id) $ toTType a