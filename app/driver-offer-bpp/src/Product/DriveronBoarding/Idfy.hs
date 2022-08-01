module Product.DriveronBoarding.Idfy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto (Transactionable (runTransaction))
import qualified Data.Text as T
import Data.Time.Format
import Domain.Types.Driveronboarding.VehicleRegistrationCert
import Environment
import Storage.Queries.Driveronboarding.DriverDrivingLicense (updateDLDetails)
import Storage.Queries.Driveronboarding.VehicleRegistrationCert (updateRCDetails)
import Types.API.Idfy
import Utils.Common

idfyDrivingLicense :: IdfyDLReq -> FlowHandler AckResponse
idfyDrivingLicense req = withFlowHandlerAPI $ do
  now <- getCurrentTime
  let validFrom = getUTCTimeFromDate req.result.source_output.nt_validity_from
  let validTill = getUTCTimeFromDate req.result.source_output.nt_validity_to
  let cov = foldr f [] req.result.source_output.field
  let idfyStatus = req.status
  let verificationStatus = validateDLStatus validTill cov req.status now
  runTransaction $ updateDLDetails req.request_id validFrom validTill idfyStatus verificationStatus (Just [LMV]) now
  pure Ack
  where
    f = \x acc -> x.cov : acc

idfyRCLicense :: IdfyRCReq -> FlowHandler AckResponse
idfyRCLicense req = withFlowHandlerAPI $ do
  now <- getCurrentTime
  let fitness_upto_ = getUTCTimeFromDate req.result.extraction_output.fitness_upto
  let insurance_validity_ = getUTCTimeFromDate req.result.extraction_output.insurance_validity
  let permit_validity_ = getUTCTimeFromDate req.result.extraction_output.permit_validity
  let registration_date_ = getUTCTimeFromDate req.result.extraction_output.registration_date
  let vehicle_class_ = req.result.extraction_output.vehicle_class
  let idfyStatus = req.status
  let verificationStatus = validateRCStatus permit_validity_ insurance_validity_ vehicle_class_ req.status now
  runTransaction $
    updateRCDetails req.request_id registration_date_ permit_validity_ fitness_upto_ insurance_validity_ idfyStatus verificationStatus (Just LMV) now
  pure Ack

getUTCTimeFromDate :: Maybe Text -> Maybe UTCTime
getUTCTimeFromDate = \case
  Nothing -> Nothing
  Just date -> do
    parseTimeM True defaultTimeLocale "%Y-%-m-%-d" $ T.unpack date

-- validateDL:
--     dl expiry
--     dl status
--     class of vehicle should be LMV or higher

--add validation for class of vehicle
validateDLStatus :: Maybe UTCTime -> [Text] -> IdfyStatus -> UTCTime -> VerificationStatus
validateDLStatus validTill cov status now = case status of
  COMPLETED -> do
    let tr = idfyValidityStatus now validTill
    let cr = foldr' (\x acc -> isValidCOV x || acc) False cov
    if tr && cr then VALID else INVALID
  FAILED -> INVALID
  IN_PROGRESS -> PENDING

-- validate:
--         rc exp. -> using permit validity
--         rc status -> using status
--         insurance validity
--         vehicle class

--add validation for class of vehicle
validateRCStatus :: Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> IdfyStatus -> UTCTime -> VerificationStatus
validateRCStatus rcExpiry insuranceValidity cov status now = case status of
  COMPLETED -> do
    let tr = idfyValidityStatus now rcExpiry
    let cr = maybe False isValidCOV cov
    let ir = idfyValidityStatus now insuranceValidity
    if tr && ir && cr then VALID else INVALID
  FAILED -> INVALID
  IN_PROGRESS -> PENDING

idfyValidityStatus :: UTCTime -> Maybe UTCTime -> Bool
idfyValidityStatus now = maybe False (now <=)

inValidCov :: [Text]
inValidCov = ["3W_NT", "3W_T", "3W_CAB", "MCWG", "MCWOG", "MGV", "MMV"]

isValidCOV :: Text -> Bool
isValidCOV cov = foldr' (\x acc -> x /= cov && acc) True inValidCov