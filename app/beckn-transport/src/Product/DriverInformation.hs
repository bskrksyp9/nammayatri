module Product.DriverInformation where

import App.Types
import qualified App.Types as App
import Beckn.External.Encryption (decrypt)
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Amount (amountToString)
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Utils.Validation
import EulerHS.Prelude hiding (id)
import qualified Product.Location as Location
import Product.Person (sendInviteSms)
import qualified Product.Person as Person
import qualified Product.Registration as Registration
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Location as QLocation
import qualified Storage.Queries.NotificationStatus as QNotificationStatus
import qualified Storage.Queries.Organization as QOrganization
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ProductInstance as QueryPI
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Types.API.DriverInformation as DriverInformationAPI
import Types.API.Person (createPerson)
import Types.API.Registration (makeUserInfoRes)
import Types.API.Vehicle (createVehicle)
import Types.App
import Types.Error
import qualified Types.Storage.DriverInformation as DriverInfo
import qualified Types.Storage.Person as SP
import Types.Storage.RegistrationToken (RegistrationToken, RegistrationTokenT (..))
import Utils.Common (fromMaybeM, throwError, withFlowHandlerAPI)

createDriver :: Text -> DriverInformationAPI.CreateDriverReq -> FlowHandler DriverInformationAPI.CreateDriverRes
createDriver orgId req = withFlowHandlerAPI $ do
  runRequestValidation DriverInformationAPI.validateCreateDriverReq req
  let personEntity = req.person
  validateDriver personEntity
  validateVehicle req.vehicle
  person <- createPerson req.person (Id orgId)
  vehicle <- createVehicle req.vehicle (Id orgId)
  DB.runSqlDBTransaction $ do
    QPerson.create person
    createDriverDetails (person.id)
    QVehicle.create vehicle
    QPerson.updateVehicle person.id $ Just vehicle.id
  org <-
    QOrganization.findOrganizationById (Id orgId)
      >>= fromMaybeM OrgNotFound
  decPerson <- decrypt person
  let Just mobNum = personEntity.mobileNumber -- already checked
      Just mobCounCode = personEntity.mobileCountryCode
  smsCfg <- smsCfg <$> ask
  inviteSmsTemplate <- inviteSmsTemplate <$> ask
  sendInviteSms smsCfg inviteSmsTemplate (mobCounCode <> mobNum) (org.name)
  return . DriverInformationAPI.CreateDriverRes $ makeUserInfoRes decPerson
  where
    validateDriver preq =
      case (preq.mobileNumber, preq.mobileCountryCode) of
        (Just mobileNumber, Just countryCode) ->
          whenM (isJust <$> QPerson.findByMobileNumber countryCode mobileNumber) $
            throwError $ InvalidRequest "Driver with this mobile number already exists."
        _ -> throwError $ InvalidRequest "You should pass mobile number and country code."
    validateVehicle preq =
      whenM (isJust <$> QVehicle.findByRegistrationNo preq.registrationNo) $
        throwError $ InvalidRequest "Vehicle with this registration number already exists."

createDriverDetails :: Id SP.Person -> DB.SqlDB ()
createDriverDetails personId = do
  now <- getCurrentTime
  let driverInfo =
        DriverInfo.DriverInformation
          { driverId = personId,
            active = False,
            onRide = False,
            createdAt = now,
            updatedAt = now
          }
  QDriverStats.createInitialDriverStats driverId
  QDriverInformation.create driverInfo
  where
    driverId = cast personId

getInformation :: RegistrationToken -> App.FlowHandler DriverInformationAPI.DriverInformationResponse
getInformation RegistrationToken {..} = withFlowHandlerAPI $ do
  _ <- Registration.checkPersonExists entityId
  let driverId = Id entityId
  person <- QPerson.findPersonById (Id entityId) >>= fromMaybeM PersonNotFound
  personEntity <- Person.mkPersonRes person
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  organization <-
    QOrganization.findOrganizationById orgId
      >>= fromMaybeM OrgNotFound
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  pure $
    DriverInformationAPI.DriverInformationResponse
      { transporter = organization,
        person = personEntity,
        driverInformation = driverInfo
      }

setActivity :: RegistrationToken -> Bool -> App.FlowHandler APISuccess.APISuccess
setActivity RegistrationToken {..} isActive = withFlowHandlerAPI $ do
  _ <- Registration.checkPersonExists entityId
  let driverId = Id entityId
  QDriverInformation.updateActivity driverId isActive
  pure APISuccess.Success

getRideInfo :: RegistrationToken -> Maybe (Id Ride) -> App.FlowHandler DriverInformationAPI.GetRideInfoRes
getRideInfo RegistrationToken {..} rideId = withFlowHandlerAPI $ do
  mbNotification <- QNotificationStatus.findActiveNotificationByDriverId driverId rideId
  case mbNotification of
    Nothing -> return $ DriverInformationAPI.GetRideInfoRes Nothing
    Just notification -> do
      let productInstanceId = cast $ notification.rideId
      let notificationExpiryTime = notification.expiresAt
      productInstance <- QueryPI.findById productInstanceId >>= fromMaybeM PINotFound
      driver <- QPerson.findPersonById (cast driverId) >>= fromMaybeM PersonNotFound
      driverLocation <- findLocationById (driver.locationId) >>= fromMaybeM (PersonFieldNotPresent "location_id")
      fromLocation <- findLocationById (productInstance.fromLocation) >>= fromMaybeM (PIFieldNotPresent "location_id")
      toLocation <- findLocationById (productInstance.toLocation) >>= fromMaybeM (PIFieldNotPresent "to_location_id")
      (fromLat, fromLong) <- extractLatLong fromLocation & fromMaybeM (LocationFieldNotPresent "from")
      (driverLat, driverLong) <- extractLatLong driverLocation & fromMaybeM (LocationFieldNotPresent "driver")
      mbRoute <- Location.getRoute' driverLat driverLong fromLat fromLong
      return $
        DriverInformationAPI.GetRideInfoRes $
          Just $
            DriverInformationAPI.RideInfo
              { productInstanceId = productInstanceId,
                pickupLoc = fromLocation,
                dropLoc = toLocation,
                etaForPickupLoc = (`div` 60) . durationInS <$> mbRoute,
                distanceToPickupLoc = distanceInM <$> mbRoute,
                notificationExpiryTime = notificationExpiryTime,
                estimatedPrice = amountToString <$> productInstance.price
              }
  where
    driverId = Id entityId
    findLocationById mbId = maybe (return Nothing) QLocation.findLocationById mbId
    extractLatLong = \loc -> (,) <$> loc.lat <*> loc.long

listDriver :: Text -> Maybe Integer -> Maybe Integer -> FlowHandler DriverInformationAPI.ListDriverRes
listDriver orgId mbLimit mbOffset = withFlowHandlerAPI $ do
  personList <- QDriverInformation.findAllWithLimitOffsetByOrgIds mbLimit mbOffset [Id orgId]
  respPersonList <- traverse convertToRes personList
  return $ DriverInformationAPI.ListDriverRes respPersonList
  where
    convertToRes (person, driverInfo) = do
      vehicle <- maybe (return Nothing) QVehicle.findVehicleById $ Id <$> person.udf1
      decMobNum <- decrypt person.mobileNumber
      return $
        DriverInformationAPI.DriverEntityRes
          { id = person.id,
            firstName = person.firstName,
            middleName = person.middleName,
            lastName = person.lastName,
            mobileNumber = decMobNum,
            linkedVehicle = vehicle,
            active = driverInfo.active,
            onRide = driverInfo.onRide
          }

linkVehicle :: Text -> Id SP.Person -> DriverInformationAPI.LinkVehicleReq -> FlowHandler DriverInformationAPI.LinkVehicleRes
linkVehicle orgId personId req = withFlowHandlerAPI $ do
  person <-
    QPerson.findPersonById personId
      >>= fromMaybeM PersonDoesNotExist
  vehicle <-
    QVehicle.findVehicleById (req.vehicleId)
      >>= fromMaybeM VehicleDoesNotExist
  unless
    ( person.organizationId == Just (Id orgId)
        && vehicle.organizationId == Id orgId
    )
    (throwError Unauthorized)
  prevPerson <- QPerson.findByVehicleId $ req.vehicleId
  whenJust prevPerson $ \_ -> throwError VehicleAlreadyLinked
  QPerson.updateVehicleFlow personId $ Just (req.vehicleId)
  return APISuccess.Success
