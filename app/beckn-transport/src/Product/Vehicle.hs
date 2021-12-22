module Product.Vehicle where

import App.Types
import Beckn.Types.APISuccess
import Beckn.Types.Id
import Beckn.Utils.Validation (runRequestValidation)
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QV
import Types.API.Vehicle as API
import Types.Error
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as SP
import qualified Types.Storage.Vehicle as SV
import Utils.Common
import qualified Utils.Defaults as Default

createVehicle :: SP.Person -> CreateVehicleReq -> FlowHandler CreateVehicleRes
createVehicle admin req = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  runRequestValidation validateCreateVehicleReq req
  validateVehicle
  vehicle <- API.createVehicle req orgId
  QV.createFlow vehicle
  vehAPIEntity <- SV.buildVehicleAPIEntity vehicle
  return $ CreateVehicleRes vehAPIEntity
  where
    validateVehicle = do
      mVehicle <- QV.findByRegistrationNo $ req.registrationNo
      when (isJust mVehicle) $
        throwError $ InvalidRequest "Registration number already exists."

listVehicles :: SP.Person -> SV.Variant -> Maybe SV.Category -> Maybe SV.EnergyType -> Maybe Text -> Maybe Int -> Maybe Int -> FlowHandler ListVehicleRes
listVehicles admin variant categoryM energyTypeM mbRegNum limitM offsetM = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  personList <- QP.findAllByOrgId [SP.DRIVER] orgId
  vehicleList <- QV.findAllByVariantCatOrgId variant categoryM energyTypeM mbRegNum limit offset orgId
  respList <- buildVehicleRes personList `traverse` vehicleList
  return $ ListVehicleRes respList
  where
    limit = toInteger $ fromMaybe Default.limit limitM
    offset = toInteger $ fromMaybe Default.offset offsetM

updateVehicle :: SP.Person -> Id SV.Vehicle -> UpdateVehicleReq -> FlowHandler UpdateVehicleRes
updateVehicle admin vehicleId req = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  runRequestValidation validateUpdateVehicleReq req
  vehicle <- QV.findByIdAndOrgId vehicleId orgId >>= fromMaybeM VehicleDoesNotExist
  let updatedVehicle =
        vehicle{variant = fromMaybe vehicle.variant req.variant,
                model = fromMaybe vehicle.model req.model,
                color = fromMaybe vehicle.color req.color,
                category = req.category <|> vehicle.category
               }
  QV.updateVehicleRec updatedVehicle
  SV.buildVehicleAPIEntity vehicle

deleteVehicle :: SP.Person -> Id SV.Vehicle -> FlowHandler DeleteVehicleRes
deleteVehicle admin vehicleId = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  vehicle <-
    QV.findVehicleById vehicleId
      >>= fromMaybeM VehicleDoesNotExist
  unless (vehicle.organizationId == orgId) $ throwError Unauthorized
  QV.deleteById vehicleId
  return Success

getVehicle :: Id SP.Person -> Maybe Text -> Maybe (Id SV.Vehicle) -> FlowHandler CreateVehicleRes
getVehicle personId registrationNoM vehicleIdM = withFlowHandlerAPI $ do
  user <-
    QP.findPersonById personId
      >>= fromMaybeM PersonNotFound
  vehicle <- case (registrationNoM, vehicleIdM) of
    (Nothing, Nothing) -> throwError $ InvalidRequest "You should pass registration number and vehicle id."
    _ ->
      QV.findByAnyOf registrationNoM vehicleIdM
        >>= fromMaybeM VehicleDoesNotExist
  hasAccess user vehicle
  vehAPIEntity <- SV.buildVehicleAPIEntity vehicle
  return $ CreateVehicleRes vehAPIEntity
  where
    hasAccess user vehicle =
      when (user.organizationId /= Just (vehicle.organizationId)) $
        throwError Unauthorized

addOrgId :: Id Org.Organization -> SV.Vehicle -> SV.Vehicle
addOrgId orgId vehicle = vehicle {SV.organizationId = orgId}

buildVehicleRes :: MonadFlow m => [SP.Person] -> SV.Vehicle -> m VehicleRes
buildVehicleRes personList vehicle = do
  let mdriver =
        find
          ( \person ->
              person.udf1 == Just (getId $ vehicle.id)
          )
          personList
  vehAPIEntity <- SV.buildVehicleAPIEntity vehicle
  return
    VehicleRes
      { vehicle = vehAPIEntity,
        driver = mkDriverObj <$> mdriver
      }

mkDriverObj :: SP.Person -> Driver
mkDriverObj person =
  Driver
    { id = getId $ person.id,
      firstName = person.firstName,
      middleName = person.middleName,
      lastName = person.lastName,
      fullName = person.fullName,
      rating = round <$> person.rating,
      organizationId = person.organizationId
    }
