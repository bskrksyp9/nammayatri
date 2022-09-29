module SharedLogic.FareCalculator.Flow
  ( calculateFare,
    doCalculateFare,
    fareSum,
    baseFareSum,
    calculateFareParameters,
    mkBreakupList,
  )
where

import Beckn.Storage.Esqueleto (Transactionable)
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.FareParams
import Domain.Types.FarePolicy (FarePolicy)
import Domain.Types.Organization (Organization)
import Domain.Types.Vehicle.Variant (Variant)
import EulerHS.Prelude hiding (id)
import SharedLogic.FareCalculator.Calculator
  ( baseFareSum,
    calculateFareParameters,
    fareSum,
    mkBreakupList,
  )
import qualified Storage.Queries.FarePolicy as FarePolicyS
import Tools.Error

type MonadHandler m = (MonadThrow m, Log m)

newtype ServiceHandle m = ServiceHandle
  { getFarePolicy :: Id Organization -> Variant -> m (Maybe FarePolicy)
  }

serviceHandle :: Transactionable m => ServiceHandle m
serviceHandle =
  ServiceHandle
    { getFarePolicy = FarePolicyS.findFarePolicyByOrgAndVariant
    }

calculateFare ::
  (Transactionable m, MonadFlow m) =>
  Id Organization ->
  Variant ->
  Meters ->
  UTCTime ->
  Maybe Money ->
  m FareParameters
calculateFare = doCalculateFare serviceHandle

doCalculateFare ::
  MonadHandler m =>
  ServiceHandle m ->
  Id Organization ->
  Variant ->
  Meters ->
  UTCTime ->
  Maybe Money ->
  m FareParameters
doCalculateFare ServiceHandle {..} orgId variant distance endTime driverSelectedFare = do
  logTagInfo "FareCalculator" $ "Initiating fare calculation for organization " +|| orgId ||+ ""
  farePolicy <- getFarePolicy orgId variant >>= fromMaybeM NoFarePolicy
  let fareParams = calculateFareParameters farePolicy distance endTime driverSelectedFare
  logTagInfo
    "FareCalculator"
    $ "Fare parameters calculated: " +|| fareParams ||+ ""
  pure fareParams