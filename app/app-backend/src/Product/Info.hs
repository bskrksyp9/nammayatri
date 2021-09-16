module Product.Info where

import App.Types
import Beckn.Types.Id
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.ProductInstance as MPI
import Types.API.Location
import Types.API.Product
import Types.Error
import Types.ProductInfo as ProductInfo
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Case as SC
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as SPI
import Utils.Common

getProductInfo :: Id Person.Person -> Id SPI.ProductInstance -> FlowHandler GetProductInfoRes
getProductInfo personId prodInstId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  productInstance <- MPI.findById prodInstId >>= fromMaybeM PIDoesNotExist
  case decodeFromText =<< SPI.info productInstance of
    Just info ->
      case ProductInfo.tracker info of
        Nothing -> throwError $ PIFieldNotPresent "tracker"
        Just tracker -> do
          let trip = ProductInfo.trip tracker
          return $
            GetProductInfoRes
              { vehicle = trip.vehicle,
                driver = trip.driver,
                travellers = trip.travellers,
                fare = trip.fare,
                caseId = getId (SPI.caseId productInstance),
                product = productInstance
              }
    Nothing ->
      logTagInfo "get Product info" "No info found in products table"
        >> throwError (PIFieldNotPresent "info")

-- TODO: fetch tracking URL from tracker info
getLocation :: Id Person.Person -> Id SC.Case -> FlowHandler GetLocationRes
getLocation personId searchCaseId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  _ <- Case.findIdByPersonId personId searchCaseId >>= fromMaybeM CaseDoesNotExist
  baseUrl <- xProviderUri <$> ask
  orderCase <- Case.findOneByParentIdAndCaseType searchCaseId Case.RIDEORDER >>= fromMaybeM CaseDoesNotExist
  orderPI <- MPI.findOneByCaseId orderCase.id >>= fromMaybeM PIDoesNotExist
  orderPIid <- orderPI.parentId & fromMaybeM (PIFieldNotPresent "parentId")
  ExternalAPI.location baseUrl (getId orderPIid)
