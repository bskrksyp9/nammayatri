module Types.API.FarePolicy
  ( ListFarePolicyRes (..),
    UpdateFarePolicyReq (..),
    UpdateFarePolicyRes,
    validateUpdateFarePolicyRequest,
  )
where

import Beckn.Types.APISuccess
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import Data.Time (TimeOfDay (..))
import Domain.Types.FarePolicy (FarePolicyAPIEntity)
import Domain.Types.FarePolicy.PerExtraKmRate (PerExtraKmRateAPIEntity, validatePerExtraKmRateAPIEntity)
import EulerHS.Prelude hiding (id)

newtype ListFarePolicyRes = ListFarePolicyRes
  { oneWayFarePolicies :: [FarePolicyAPIEntity]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data UpdateFarePolicyReq = UpdateFarePolicyReq
  { baseFare :: Maybe Double,
    perExtraKmRateList :: NonEmpty PerExtraKmRateAPIEntity,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Double
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type UpdateFarePolicyRes = APISuccess

validateUpdateFarePolicyRequest :: Validate UpdateFarePolicyReq
validateUpdateFarePolicyRequest UpdateFarePolicyReq {..} =
  sequenceA_
    [ validateField "baseFare" baseFare . InMaybe $ InRange @Double 0 500,
      validateList "perExtraKmRateList" perExtraKmRateList validatePerExtraKmRateAPIEntity,
      validateField "perExtraKmRateList" perExtraKmRateList $ UniqueField @"distanceRangeStart",
      validateField "nightShiftRate" nightShiftRate . InMaybe $ InRange @Double 1 2,
      validateField "nightShiftStart" nightShiftStart . InMaybe $ InRange (TimeOfDay 18 0 0) (TimeOfDay 23 30 0),
      validateField "nightShiftEnd" nightShiftEnd . InMaybe $ InRange (TimeOfDay 0 30 0) (TimeOfDay 7 0 0)
    ]