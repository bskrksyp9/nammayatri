module Product.RideAPI.EndRide where

import App.Types (FlowHandler)
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import qualified Product.FareCalculator.Interpreter as Fare
import Product.ProductInstance as PI
import qualified Product.RideAPI.Handlers.EndRide as Handler
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as PI
import Types.App (Driver)
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Person as SP
import qualified Types.Storage.ProductInstance as PI
import Utils.Common (withFlowHandlerAPI)
import Utils.Metrics (putFareAndDistanceDeviations)

endRide :: Id SP.Person -> Id PI.ProductInstance -> FlowHandler APISuccess.APISuccess
endRide personId rideId = withFlowHandlerAPI $ do
  Handler.endRideHandler handle personId rideId
  where
    handle =
      Handler.ServiceHandle
        { findPersonById = Person.findPersonById,
          findPIById = PI.findById',
          findAllPIByParentId = PI.findAllByParentId,
          findCaseByIdAndType = Case.findByIdType,
          notifyUpdateToBAP = PI.notifyUpdateToBAP,
          endRideTransaction,
          calculateFare = \orgId vehicleVariant distance -> Fare.calculateFare orgId vehicleVariant (Right distance),
          recalculateFareEnabled = asks (.recalculateFareEnabled),
          putDiffMetric = putFareAndDistanceDeviations
        }

endRideTransaction :: DBFlow m r => [Id PI.ProductInstance] -> Id Case.Case -> Id Case.Case -> Id Driver -> Amount -> m ()
endRideTransaction piIds trackerCaseId orderCaseId driverId actualPrice = DB.runSqlDBTransaction $ do
  forM_ piIds $ PI.updateActualPrice actualPrice
  PI.updateStatusByIds piIds PI.COMPLETED
  Case.updateStatus trackerCaseId Case.COMPLETED
  Case.updateStatus orderCaseId Case.COMPLETED
  DriverInformation.updateOnRide driverId False
  DriverStats.updateIdleTime driverId
