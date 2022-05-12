module Types.API.FarePolicy.Discount
  ( UpdateFarePolicyDiscountReq,
    UpdateFarePolicyDiscountRes,
    CreateFarePolicyDiscountReq,
    CreateFarePolicyDiscountRes,
    DeleteFarePolicyDiscountRes,
    validateCreateFarePolicyDiscountReq,
    validateUpdateFarePolicyDiscountReq,
  )
where

import Beckn.Types.APISuccess
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import qualified Domain.Types.Vehicle as Veh
import EulerHS.Prelude hiding (id)
import qualified Utils.Validation as TV

data CreateFarePolicyDiscountReq = CreateFarePolicyDiscountReq
  { vehicleVariant :: Veh.Variant,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    discount :: Double,
    enabled :: Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type CreateFarePolicyDiscountRes = APISuccess

validateCreateFarePolicyDiscountReq :: Validate CreateFarePolicyDiscountReq
validateCreateFarePolicyDiscountReq CreateFarePolicyDiscountReq {..} =
  sequenceA_
    [ validateField "discount" discount $ Min @Double 0.01,
      TV.validateDiscountDate "toDate" toDate fromDate
    ]

data UpdateFarePolicyDiscountReq = UpdateFarePolicyDiscountReq
  { fromDate :: UTCTime,
    toDate :: UTCTime,
    discount :: Double,
    enabled :: Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type UpdateFarePolicyDiscountRes = APISuccess

validateUpdateFarePolicyDiscountReq :: Validate UpdateFarePolicyDiscountReq
validateUpdateFarePolicyDiscountReq UpdateFarePolicyDiscountReq {..} =
  sequenceA_
    [ validateField "discount" discount $ Min @Double 0.01,
      TV.validateDiscountDate "toDate" toDate fromDate
    ]

type DeleteFarePolicyDiscountRes = APISuccess
