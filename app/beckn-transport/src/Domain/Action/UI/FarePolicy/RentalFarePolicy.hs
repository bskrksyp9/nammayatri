{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.FarePolicy.RentalFarePolicy
  ( ListRentalFarePoliciesRes (..),
    CreateRentalFarePolicyReq (..),
    CreateRentalFarePolicyItem (..),
    createRentalFarePolicy,
    listRentalFarePolicies,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess
import Beckn.Types.Common
import Beckn.Types.Id (Id (..))
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Domain.Types.FarePolicy.RentalFarePolicy as Domain
import Domain.Types.Organization
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle as Vehicle
import qualified Storage.Queries.FarePolicy.RentalFarePolicy as SRentalFarePolicy
import Types.Error
import Utils.Common

newtype ListRentalFarePoliciesRes = ListRentalFarePoliciesRes
  { rentalFarePolicies :: [RentalFarePolicyAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype CreateRentalFarePolicyReq = CreateRentalFarePolicyReq
  { createList :: NonEmpty CreateRentalFarePolicyItem
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateRentalFarePolicyItem = CreateRentalFarePolicyItem
  { vehicleVariant :: Vehicle.Variant,
    baseFare :: Double,
    baseDistance :: Kilometers, -- Distance
    baseDuration :: Hours,
    extraKmFare :: Double,
    extraMinuteFare :: Double,
    driverAllowanceForDay :: Maybe Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

validateCreateRentalsFarePolicyRequest :: Validate CreateRentalFarePolicyItem
validateCreateRentalsFarePolicyRequest CreateRentalFarePolicyItem {..} =
  sequenceA_
    [ validateField "baseFare" baseFare $ Min @Double 0,
      validateField "baseDistance" baseDistance $ Min @Kilometers 0,
      validateField "baseDuration" baseDuration $ Min @Hours 0,
      validateField "extraKmFare" extraKmFare $ Min @Double 0,
      validateField "extraMinuteFare" extraMinuteFare $ Min @Double 0,
      validateField "driverAllowanceForDay" driverAllowanceForDay $ InMaybe $ Min @Double 0
    ]

createRentalFarePolicy :: (EsqDBFlow m r) => SP.Person -> CreateRentalFarePolicyReq -> m APISuccess
createRentalFarePolicy admin req = do
  orgId <- admin.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  mapM_ (runRequestValidation validateCreateRentalsFarePolicyRequest) req.createList
  newRentalFarePolicyItems <- forM req.createList $ \createItemReq -> do
    guid <- Id <$> generateGUID
    pure $ toDomainType orgId guid createItemReq
  Esq.runTransaction $ do
    SRentalFarePolicy.markAllAsDeleted orgId
    forM_ newRentalFarePolicyItems SRentalFarePolicy.create
  pure Success
  where
    toDomainType :: Id Organization -> Id RentalFarePolicy -> CreateRentalFarePolicyItem -> RentalFarePolicy
    toDomainType orgId guid CreateRentalFarePolicyItem {..} = do
      let extraKmFare' = realToFrac extraKmFare
          extraMinuteFare' = realToFrac extraMinuteFare
          driverAllowanceForDay' = realToFrac <$> driverAllowanceForDay
      RentalFarePolicy
        { id = guid,
          organizationId = orgId,
          baseFare = realToFrac baseFare,
          extraKmFare = extraKmFare',
          extraMinuteFare = extraMinuteFare',
          driverAllowanceForDay = driverAllowanceForDay',
          descriptions = mkDescriptions extraKmFare' extraMinuteFare' driverAllowanceForDay',
          ..
        }

listRentalFarePolicies :: (EsqDBFlow m r) => SP.Person -> m ListRentalFarePoliciesRes
listRentalFarePolicies person = do
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  rentalFarePolicies <- SRentalFarePolicy.findRentalFarePoliciesByOrg orgId
  pure $
    ListRentalFarePoliciesRes
      { rentalFarePolicies = map makeRentalFarePolicyAPIEntity rentalFarePolicies
      }