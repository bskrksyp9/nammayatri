{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Product.DriveronBoarding.DriverOnBoarding where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.APISuccess
import Beckn.Types.Id
import Beckn.Types.Predicate
import Beckn.Types.Validation
import Beckn.Utils.Predicates
import Beckn.Utils.Validation
import qualified Domain.Types.Driveronboarding.DriverDrivingLicense as DDL
import qualified Domain.Types.Driveronboarding.VehicleRegistrationCert as DVR hiding (VALID)
import qualified Domain.Types.Person as SP
import Environment
import qualified EulerHS.Language as L
import qualified Storage.Queries.Driveronboarding.DriverDrivingLicense as QDDL
import qualified Storage.Queries.Driveronboarding.VehicleRegistrationCert as QVR
import qualified Storage.Queries.Person as QPerson
import Types.API.Driveronboarding.DriverOnBoarding
import Types.Error
import Utils.Common

validateDriverDrivingLicense :: Validate DriverOnBoardingReq
validateDriverDrivingLicense DriverOnBoardingReq {..} =
  sequenceA_
    [ validateField "driverLicenseNumber" driverLicenseNumber $ MinLength 5 `And` text
    ]
  where
    extractMaybe (Just x) = x
    text = star $ alphanum \/ ","

registrationHandler :: Id SP.Person -> DriverOnBoardingReq -> FlowHandler DriverOnBoardingRes
registrationHandler personId req@DriverOnBoardingReq {..} = withFlowHandlerAPI $ do
  runRequestValidation validateDriverDrivingLicense DriverOnBoardingReq {..}
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  task_id <- L.generateGUID -- task_id for idfy request
  let group_id = personId -- group_id for idfy request
  now <- getCurrentTime
  driverDLDetails <- QDDL.findByDId personId
  vehicleRCDetails <- QVR.findByPId personId
  handleDLVerification req personId driverDLDetails
  handleRCVerification req personId vehicleRCDetails
  return Success

handleDLVerification :: DriverOnBoardingReq -> Id SP.Person -> Maybe DDL.DriverDrivingLicense -> Flow ()
handleDLVerification req personId dl = do
  now <- getCurrentTime
  case dl of
    Nothing -> do
      dlId <- L.generateGUID
      let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
      dlEntity <- mkDriverDrivingLicenseEntry dlId personId (Just req.driverDateOfBirth) (Just req.driverLicenseNumber) idfyReqId now
      runTransaction $ QDDL.create dlEntity
      return ()
    Just dlRecord -> do
      dlNumber <- mapM decrypt dlRecord.driverLicenseNumber
      when (dlNumber /= Just req.driverLicenseNumber || dlRecord.driverDob /= Just req.driverDateOfBirth) do
        let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
        runTransaction $ QDDL.resetDLRequest personId (Just req.driverLicenseNumber) (Just req.driverDateOfBirth) idfyReqId now

handleRCVerification :: DriverOnBoardingReq -> Id SP.Person -> Maybe DVR.VehicleRegistrationCert -> Flow ()
handleRCVerification req personId rc = do
  now <- getCurrentTime
  case rc of
    Nothing -> do
      rcId <- L.generateGUID
      let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
      rcEntity <- mkVehicleRegistrationCertEntry rcId personId (Just req.vehicleRegistrationCertNumber) idfyReqId now
      runTransaction $ QVR.create rcEntity
      return ()
    Just rcRecord -> do
      rcNumber <- mapM decrypt rcRecord.vehicleRegistrationCertNumber
      when (rcNumber /= Just req.vehicleRegistrationCertNumber) do
        let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
        runTransaction $ QVR.resetRCRequest personId (Just req.driverLicenseNumber) idfyReqId now

mkVehicleRegistrationCertEntry :: EncFlow m r => Text -> Id SP.Person -> Maybe Text -> Text -> UTCTime -> m DVR.VehicleRegistrationCert
mkVehicleRegistrationCertEntry opId personId rcNumber reqID time = do
  vrc <- mapM encrypt rcNumber
  return $
    DVR.VehicleRegistrationCert
      { id = Id opId,
        driverId = personId,
        vehicleRegistrationCertNumber = vrc,
        fitnessCertExpiry = Nothing,
        permitNumber = Nothing,
        permitStart = Nothing,
        permitExpiry = Nothing,
        vehicleClass = Nothing,
        request_id = reqID,
        vehicleNumber = Nothing,
        insuranceValidity = Nothing,
        idfyStatus = DVR.IN_PROGRESS,
        verificationStatus = DVR.PENDING,
        createdAt = time,
        updatedAt = time,
        consentTimestamp = time,
        consent = True
      }

mkDriverDrivingLicenseEntry :: EncFlow m r => Text -> Id SP.Person -> Maybe UTCTime -> Maybe Text -> Text -> UTCTime -> m DDL.DriverDrivingLicense
mkDriverDrivingLicenseEntry opId personId dob dlNumber reqId time = do
  ddl <- mapM encrypt dlNumber
  return $
    DDL.DriverDrivingLicense
      { id = Id opId,
        driverId = personId,
        driverDob = dob,
        driverLicenseNumber = ddl,
        driverLicenseStart = Nothing,
        driverLicenseExpiry = Nothing,
        classOfVehicle = Nothing,
        idfyStatus = DVR.IN_PROGRESS,
        verificationStatus = DVR.PENDING,
        driverVerificationStatus = DVR.PENDING,
        request_id = reqId,
        consent = True,
        createdAt = time,
        updatedAt = time,
        consentTimestamp = time
      }