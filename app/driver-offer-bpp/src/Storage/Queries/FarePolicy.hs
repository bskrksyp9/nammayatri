module Storage.Queries.FarePolicy
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.FarePolicy
import Domain.Types.Organization
import Domain.Types.Vehicle.Variant (Variant)
import Storage.Tabular.FarePolicy

findAllByOrgId ::
  Transactionable m =>
  Id Organization ->
  m [FarePolicy]
findAllByOrgId orgId = do
  Esq.findAll $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyOrganizationId ==. val (toKey orgId)
    return farePolicy

findByOrgIdAndVariant ::
  Transactionable m =>
  Id Organization ->
  Variant ->
  m (Maybe FarePolicy)
findByOrgIdAndVariant orgId variant = do
  Esq.findOne $ do
    farePolicy <- from $ table @FarePolicyT
    where_ $
      farePolicy ^. FarePolicyOrganizationId ==. val (toKey orgId)
        &&. farePolicy ^. FarePolicyVehicleVariant ==. val variant
    return farePolicy

findById :: Transactionable m => Id FarePolicy -> m (Maybe FarePolicy)
findById = Esq.findById

update :: FarePolicy -> SqlDB ()
update farePolicy = do
  now <- getCurrentTime
  void $
    Esq.update $ \tbl -> do
      set
        tbl
        [ FarePolicyBaseDistanceFare =. val farePolicy.baseDistanceFare,
          FarePolicyBaseDistanceMeters =. val farePolicy.baseDistanceMeters,
          FarePolicyPerExtraKmFare =. val farePolicy.perExtraKmFare,
          FarePolicyDeadKmFare =. val farePolicy.deadKmFare,
          FarePolicyDriverMinExtraFee =. val farePolicy.driverExtraFee.minFee,
          FarePolicyDriverMaxExtraFee =. val farePolicy.driverExtraFee.maxFee,
          FarePolicyNightShiftStart =. val farePolicy.nightShiftStart,
          FarePolicyNightShiftEnd =. val farePolicy.nightShiftEnd,
          FarePolicyNightShiftRate =. val farePolicy.nightShiftRate,
          FarePolicyUpdatedAt =. val now
        ]
      where_ $ tbl ^. FarePolicyTId ==. val (toKey farePolicy.id)
