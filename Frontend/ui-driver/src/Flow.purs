{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Flow where

import Log

import Control.Monad.Except.Trans (lift)
import Data.Array (filter, head, length, null, (!!), any)
import Data.Either (Either(..))
import Data.Int (round, toNumber, ceil)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (fromString) as Number
import Data.String (Pattern(..), split, toUpper)
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (liftFlow, getNewIDWithTag, bundleVersion, os, getExpiryTime)
import Foreign.Class (class Encode, encode, decode)
import Helpers.Utils (getCurrentUTC, hideSplash, getTime, convertUTCtoISC, decodeErrorCode, toString, secondsLeft, decodeErrorMessage, parseFloat, getcurrentdate, getDowngradeOptions)
import JBridge (drawRoute, factoryResetApp, firebaseLogEvent, firebaseUserID, getCurrentLatLong, getCurrentPosition, getVersionCode, getVersionName, isBatteryPermissionEnabled, isInternetAvailable, isLocationEnabled, isLocationPermissionEnabled, isOverlayPermissionEnabled, loaderText, openNavigation, removeAllPolylines, removeMarker, showMarker, startLocationPollingAPI, stopLocationPollingAPI, toast, toggleLoader, generateSessionId, stopChatListenerService, hideKeyboardOnNavigation, metaLogEvent)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, discard, pure, unit, unless, void, when, ($), (==), (/=), (&&), (||), (/), when, (+), show, (>), not, (<), (*), (-), (<=), (<$>))
import Presto.Core.Types.Language.Flow (delay, setLogField)
import Presto.Core.Types.Language.Flow (doAff, fork)
import Resource.Constants (decodeAddress)
import Screens.BookingOptionsScreen.Controller (downgradeOptionsConfig)
import Screens.BookingOptionsScreen.ScreenData as BookingOptionsScreenData
import Screens.Handlers as UI
import Screens.HomeScreen.Controller (activeRideDetail)
import Screens.HomeScreen.View (rideRequestPollingData)
import Screens.PopUpScreen.Controller (transformAllocationData)
import Screens.Types (ActiveRide, AllocationData, HomeScreenStage(..), Location, KeyboardModalType(..), ReferralType(..), DriverStatus(..))
import Screens.Types as ST
import Services.APITypes (DriverActiveInactiveResp(..), DriverArrivedReq(..), DriverDLResp(..), DriverProfileStatsReq(..), DriverProfileStatsResp(..), DriverRCResp(..), DriverRegistrationStatusReq(..), DriverRegistrationStatusResp(..), GetDriverInfoReq(..), GetDriverInfoResp(..), GetRidesHistoryResp(..), GetRouteResp(..), LogOutReq(..), LogOutRes(..), OfferRideResp(..), ReferDriverResp(..), ResendOTPResp(..), RidesInfo(..), Route(..), StartRideResponse(..), Status(..), TriggerOTPResp(..), UpdateDriverInfoResp(..), ValidateImageReq(..), ValidateImageRes(..), Vehicle(..), VerifyTokenResp(..), DriverAlternateNumberResp(..), RemoveAlternateNumberRequest(..), RemoveAlternateNumberResp(..), AlternateNumberResendOTPResp(..), DriverAlternateNumberOtpResp(..), GetPerformanceReq(..), GetPerformanceRes(..))
import Services.Accessor (_lat, _lon, _id)
import Services.Backend (makeTriggerOTPReq, makeGetRouteReq, walkCoordinates, walkCoordinate, makeVerifyOTPReq, makeUpdateDriverInfoReq, makeOfferRideReq, makeDriverRCReq, makeDriverDLReq, makeValidateImageReq, makeReferDriverReq, dummyVehicleObject, driverRegistrationStatusBT, makeUpdateDriverLangChange, makeValidateAlternateNumberRequest, makeResendAlternateNumberOtpRequest, makeVerifyAlternateNumberOtpRequest, makeLinkReferralCodeReq, makeUpdateBookingOptions)
import Services.Backend as Remote
import Services.Config (getBaseUrl)
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalNativeStore, getValueToLocalStore, isLocalStageOn, setValueToLocalNativeStore, setValueToLocalStore)
import Types.App (ABOUT_US_SCREEN_OUTPUT(..), ADD_VEHICLE_DETAILS_SCREENOUTPUT(..), APPLICATION_STATUS_SCREENOUTPUT(..), DRIVER_DETAILS_SCREEN_OUTPUT(..), DRIVER_PROFILE_SCREEN_OUTPUT(..), DRIVER_RIDE_RATING_SCREEN_OUTPUT(..), ENTER_MOBILE_NUMBER_SCREEN_OUTPUT(..), ENTER_OTP_SCREEN_OUTPUT(..), FlowBT, GlobalState(..), HELP_AND_SUPPORT_SCREEN_OUTPUT(..), HOME_SCREENOUTPUT(..), MY_RIDES_SCREEN_OUTPUT(..), NOTIFICATIONS_SCREEN_OUTPUT(..), NO_INTERNET_SCREEN_OUTPUT(..), PERMISSIONS_SCREEN_OUTPUT(..), POPUP_SCREEN_OUTPUT(..), REGISTRATION_SCREENOUTPUT(..), RIDE_DETAIL_SCREENOUTPUT(..), SELECT_LANGUAGE_SCREEN_OUTPUT(..), ScreenStage(..), ScreenType(..), TRIP_DETAILS_SCREEN_OUTPUT(..), UPLOAD_ADHAAR_CARD_SCREENOUTPUT(..), UPLOAD_DRIVER_LICENSE_SCREENOUTPUT(..), VEHICLE_DETAILS_SCREEN_OUTPUT(..), WRITE_TO_US_SCREEN_OUTPUT(..), NOTIFICATIONS_SCREEN_OUTPUT(..), REFERRAL_SCREEN_OUTPUT(..), BOOKING_OPTIONS_SCREEN_OUTPUT(..), defaultGlobalState)
import Types.ModifyScreenState (modifyScreenState, updateStage)
import Screens.DriverProfileScreen.Controller (getDowngradeOptionsSelected)
import MerchantConfigs.Utils(getMerchant, Merchant(..))

baseAppFlow :: FlowBT String Unit
baseAppFlow = do
    (GlobalState state) <- getState
    versionCode <- lift $ lift $ liftFlow $ getVersionCode
    checkVersion versionCode
    cacheAppParameters versionCode
    void $ UI.splashScreen state.splashScreen
    let regToken = getValueToLocalStore REGISTERATION_TOKEN
    if isTokenValid regToken
      then do
        setValueToLocalNativeStore REGISTERATION_TOKEN regToken
        getDriverInfoFlow
      else loginFlow
    where
    cacheAppParameters :: Int -> FlowBT String Unit
    cacheAppParameters versionCode = do
      let bundle = bundleVersion unit
          driverId = (getValueToLocalStore DRIVER_ID)
      versionName <- lift $ lift $ liftFlow $ getVersionName
      setValueToLocalStore VERSION_NAME versionName
      setValueToLocalStore BUNDLE_VERSION bundle
      setValueToLocalStore BASE_URL (getBaseUrl "dummy")
      setValueToLocalStore RIDE_REQUEST_BUFFER "-3"
      setValueToLocalNativeStore BUNDLE_VERSION bundle
      setValueToLocalNativeStore GPS_METHOD "CURRENT"
      setValueToLocalNativeStore MAKE_NULL_API_CALL "NO"
      when ((getValueToLocalStore SESSION_ID == "__failed") || (getValueToLocalStore SESSION_ID == "(null)")) $ do
        setValueToLocalStore SESSION_ID (generateSessionId unit)
      void $ lift $ lift $ setLogField "driver_id" $ encode (driverId)
      void $ lift $ lift $ setLogField "app_version" $ encode (show versionCode)
      void $ lift $ lift $ setLogField "bundle_version" $ encode (bundle)
      void $ lift $ lift $ setLogField "platform" $ encode (os)

checkVersion :: Int -> FlowBT String Unit
checkVersion versioncode = do
  when (versioncode < (getLatestAndroidVersion (getMerchant unit))) $ do
    lift $ lift $ doAff do liftEffect hideSplash
    _ <- UI.handleAppUpdatePopUp
    checkVersion versioncode

getLatestAndroidVersion :: Merchant -> Int 
getLatestAndroidVersion merchant = 
  case merchant of 
    NAMMAYATRIPARTNER -> 54
    YATRIPARTNER -> 48 
    JATRISAATHIDRIVER -> 2

ifNotRegistered :: Unit -> Boolean
ifNotRegistered _ = getValueToLocalStore REGISTERATION_TOKEN == "__failed"

isTokenValid :: String -> Boolean
isTokenValid = (/=) "__failed"

loginFlow :: FlowBT String Unit
loginFlow = do
  lift $ lift $ doAff do liftEffect hideSplash
  runInternetCondition
  setValueToLocalStore LANGUAGE_KEY "EN_US"
  languageType <- UI.chooseLanguage
  mobileNo <- UI.enterMobileNumber
  case mobileNo of
    GO_TO_ENTER_OTP updateState -> do
      TriggerOTPResp triggerOtpResp <- Remote.triggerOTPBT (makeTriggerOTPReq updateState.data.mobileNumber)
      modifyScreenState $ EnterOTPScreenType (\enterOTPScreen → enterOTPScreen { data { tokenId = triggerOtpResp.authId}})
      enterOTPFlow
  where
    runInternetCondition = do
      internetAvailable <- lift $ lift $ liftFlow $ isInternetAvailable unit
      unless internetAvailable $ noInternetScreenFlow "INTERNET_ACTION"

enterOTPFlow :: FlowBT String Unit
enterOTPFlow = do
  action <- UI.enterOTP
  case action of
    DRIVER_INFO_API_CALL updatedState -> do
      void $ lift $ lift $ loaderText (getString SENDING_OTP) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      (VerifyTokenResp resp) <- Remote.verifyTokenBT (makeVerifyOTPReq updatedState.data.otp) updatedState.data.tokenId
      _ <- pure $ firebaseLogEvent "ny_driver_verify_otp"
      _ <- pure $ metaLogEvent "ny_driver_verify_otp"
      let driverId = ((resp.person)^. _id)
      _ <- lift $ lift $ setLogField "driver_id" $ encode (driverId)
      setValueToLocalStore DRIVER_ID driverId
      setValueToLocalStore REGISTERATION_TOKEN resp.token -- add from response
      void $ lift $ lift $ toggleLoader false
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT (makeUpdateDriverLangChange "")
      getDriverInfoFlow
    RETRY updatedState -> do
      modifyScreenState $ EnterOTPScreenType (\enterOTPScreen -> updatedState)
      (ResendOTPResp resp_resend) <- Remote.resendOTPBT updatedState.data.tokenId
      pure $ toast (getString OTP_RESENT)
      modifyScreenState $ EnterOTPScreenType (\enterOTPScreen → enterOTPScreen { data { tokenId = resp_resend.authId, attemptCount = resp_resend.attempts}})
      enterOTPFlow

-- TODO :: As per the api response check the floe for driver registration
registrationStatusFlow :: FlowBT String Unit
registrationStatusFlow = do
  let startTime = getTime unit
  -- resp <- Remote.driverRegistrationStatus (DriverResgistrationStatusReq {})
  let endTime = getTime unit
  let registrationStatus = driverRegistrationStatus
  if registrationStatus then currentRideFlow else permissionsScreenFlow

-- TODO :: modify this function as per the response. Currently DEFINED for testing purpose only
driverRegistrationStatus :: Boolean
driverRegistrationStatus = do
  (getValueToLocalStore TEST_FLOW_FOR_REGISTRATOION) == "COMPLETED"


getDriverInfoFlow :: FlowBT String Unit
getDriverInfoFlow = do
  _ <- pure $ delay $ Milliseconds 1.0
  _ <- pure $ printLog "Registration token" (getValueToLocalStore REGISTERATION_TOKEN)
  getDriverInfoApiResp <- lift $ lift $ Remote.getDriverInfoApi (GetDriverInfoReq{})
  case getDriverInfoApiResp of
    Right getDriverInfoResp -> do
      let (GetDriverInfoResp getDriverInfoResp) = getDriverInfoResp
      modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen -> applicationStatusScreen {props{alternateNumberAdded = isJust getDriverInfoResp.alternateNumber}})
      if getDriverInfoResp.enabled then do
        setValueToLocalStore IS_DRIVER_ENABLED "true"
        let (Vehicle linkedVehicle) = (fromMaybe dummyVehicleObject getDriverInfoResp.linkedVehicle)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data = homeScreen.data {driverName = getDriverInfoResp.firstName, vehicleType = linkedVehicle.variant ,  driverAlternateMobile =getDriverInfoResp.alternateNumber   }
                                                                           , props {statusOnline =  if (isJust getDriverInfoResp.mode) then
                                                                                                        (any( _ == (updateDriverStatus getDriverInfoResp.active))[Online, Silent])
                                                                                                    else getDriverInfoResp.active
                                                                                    }
                                                                            }
                                                )
        modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> driverDetailsScreen
         {data = driverDetailsScreen.data { driverName = getDriverInfoResp.firstName,
        driverVehicleType = linkedVehicle.variant,
        driverRating = getDriverInfoResp.rating,
       -- base64Image = updatedState.data.base64Image,
        driverMobile = getDriverInfoResp.mobileNumber,
        driverAlternateMobile = getDriverInfoResp.alternateNumber
        }})
        modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {data = driverProfileScreen.data {driverName = getDriverInfoResp.firstName, driverVehicleType = linkedVehicle.variant, driverRating = getDriverInfoResp.rating , driverAlternateNumber = getDriverInfoResp.alternateNumber, capacity = fromMaybe 2 linkedVehicle.capacity, downgradeOptions = getDowngradeOptions linkedVehicle.variant}})
        permissionsGiven <- checkAll3Permissions
        if permissionsGiven
          then currentRideFlow
          else permissionsScreenFlow
        else do
          setValueToLocalStore IS_DRIVER_ENABLED "false"
          if getDriverInfoResp.verified then do
            setValueToLocalStore IS_DRIVER_VERIFIED "true"
            applicationSubmittedFlow "ApprovedScreen"
            else do
              setValueToLocalStore IS_DRIVER_VERIFIED "false"
              onBoardingFlow
    Left errorPayload -> do
      if ((decodeErrorCode errorPayload.response.errorMessage) == "VEHICLE_NOT_FOUND" || (decodeErrorCode errorPayload.response.errorMessage) == "DRIVER_INFORMATON_NOT_FOUND")
        then onBoardingFlow
        else do
          _ <- pure $ toast $ getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
          if getValueToLocalStore IS_DRIVER_ENABLED == "true" then do
            permissionsGiven <- checkAll3Permissions
            if permissionsGiven then
              currentRideFlow
              else permissionsScreenFlow
            else do
              if getValueToLocalStore IS_DRIVER_VERIFIED == "true" then do
                applicationSubmittedFlow "ApprovedScreen"
                else do
                onBoardingFlow


onBoardingFlow :: FlowBT String Unit
onBoardingFlow = do
  _ <- pure $ hideKeyboardOnNavigation true
  (DriverRegistrationStatusResp resp ) <- driverRegistrationStatusBT (DriverRegistrationStatusReq { })
  lift $ lift $ doAff do liftEffect hideSplash
  if (resp.dlVerificationStatus == "NO_DOC_AVAILABLE" && resp.rcVerificationStatus == "NO_DOC_AVAILABLE") then do
    flow <- UI.registration
    case flow of
      UPLOAD_DRIVER_LICENSE -> do
        modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {rcVerificationStatus = resp.rcVerificationStatus}}
        uploadDrivingLicenseFlow
    else if(resp.dlVerificationStatus == "NO_DOC_AVAILABLE") then do
      modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {rcVerificationStatus = resp.rcVerificationStatus}})
      uploadDrivingLicenseFlow
      else if (resp.rcVerificationStatus == "NO_DOC_AVAILABLE") then addVehicleDetailsflow
        else applicationSubmittedFlow "StatusScreen"

uploadDrivingLicenseFlow :: FlowBT String Unit
uploadDrivingLicenseFlow = do
  (GlobalState state) <- getState
  flow <- UI.uploadDrivingLicense
  case flow of
    ADD_VEHICLE_DETAILS_SCREEN state -> do
      if (state.data.imageFront == "IMAGE_NOT_VALIDATED") then do
        modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {dateOfIssue = Just ""}}
        uploadDrivingLicenseFlow
        else do
          void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
          void $ lift $ lift $ toggleLoader true
          registerDriverDLResp <- lift $ lift $ Remote.registerDriverDL (makeDriverDLReq state.data.driver_license_number state.data.dob state.data.dateOfIssue state.data.imageIDFront state.data.imageIDFront)
          case registerDriverDLResp of
            Right (DriverDLResp resp) -> do
              void $ lift $ lift $ toggleLoader false
              setValueToLocalStore DOCUMENT_UPLOAD_TIME (getCurrentUTC "")
              if state.data.rcVerificationStatus /= "NO_DOC_AVAILABLE" then applicationSubmittedFlow "StatusScreen" else addVehicleDetailsflow
            Left errorPayload -> do
              void $ lift $ lift $ toggleLoader false
              modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {dateOfIssue = Just ""}}
              if errorPayload.code == 400 || (errorPayload.code == 500 && (decodeErrorCode errorPayload.response.errorMessage) == "UNPROCESSABLE_ENTITY") then do
                let correspondingErrorMessage =  Remote.getCorrespondingErrorMessage $ decodeErrorCode errorPayload.response.errorMessage
                modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { props {errorVisibility = true}, data {errorMessage = correspondingErrorMessage}}
                uploadDrivingLicenseFlow
                else do
                  _ <- pure $ toast $ getString SOMETHING_WENT_WRONG
                  uploadDrivingLicenseFlow

    LOGOUT_ACCOUNT -> do
      (LogOutRes resp) <- Remote.logOutBT LogOutReq
      deleteValueFromLocalStore REGISTERATION_TOKEN
      deleteValueFromLocalStore LANGUAGE_KEY
      deleteValueFromLocalStore VERSION_NAME
      deleteValueFromLocalStore BASE_URL
      deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
      deleteValueFromLocalStore IS_DRIVER_ENABLED
      deleteValueFromLocalStore BUNDLE_VERSION
      deleteValueFromLocalStore DRIVER_ID
      deleteValueFromLocalStore SET_ALTERNATE_TIME

      pure $ factoryResetApp ""
      _ <- pure $ firebaseLogEvent "logout"
      loginFlow

    VALIDATE_IMAGE_API state -> do
      void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_VALIDATING_THE_IMAGE)
      void $ lift $ lift $ toggleLoader true
      validateImageResp <- lift $ lift $ Remote.validateImage (makeValidateImageReq (if state.props.clickedButtonType == "front" then state.data.imageFront else state.data.imageBack) "DriverLicense")
      case validateImageResp of
       Right (ValidateImageRes resp) -> do
        void $ lift $ lift $ toggleLoader false
        modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen -> state{props{errorVisibility = false}})
        if state.props.clickedButtonType == "front" then
          modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {imageIDFront = resp.imageId}})
          else
            modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {imageIDBack = resp.imageId}})
        uploadDrivingLicenseFlow
       Left errorPayload -> do
        void $ lift $ lift $ toggleLoader false
        if errorPayload.code == 429 && (decodeErrorCode errorPayload.response.errorMessage) == "IMAGE_VALIDATION_EXCEED_LIMIT" then do
          modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { props { openGenericMessageModal = true}}
          uploadDrivingLicenseFlow
          else if errorPayload.code == 400 || (errorPayload.code == 500 && (decodeErrorCode errorPayload.response.errorMessage) == "UNPROCESSABLE_ENTITY") then do
            let correspondingErrorMessage =  Remote.getCorrespondingErrorMessage $ decodeErrorCode errorPayload.response.errorMessage
            modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { props {errorVisibility = true}, data {errorMessage = correspondingErrorMessage, imageFrontUrl = state.data.imageFront, imageFront = "IMAGE_NOT_VALIDATED"}}
            uploadDrivingLicenseFlow
            else do
              _ <- pure $ toast $ getString SOMETHING_WENT_WRONG
              modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {imageFrontUrl = state.data.imageFront, imageFront = "IMAGE_NOT_VALIDATED"}}
              uploadDrivingLicenseFlow

    GOTO_VEHICLE_DETAILS_SCREEN -> addVehicleDetailsflow
    GOTO_ONBOARDING_FLOW -> onBoardingFlow


addVehicleDetailsflow :: FlowBT String Unit
addVehicleDetailsflow = do
  flow <- UI.addVehicleDetails
  case flow of
    GO_TO_APPLICATION_SCREEN state -> do
      if (state.data.rcImageID == "IMAGE_NOT_VALIDATED") then do
        modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { data { dateOfRegistration = Just ""}}
        addVehicleDetailsflow
        else do
          void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
          void $ lift $ lift $ toggleLoader true
          registerDriverRCResp <- lift $ lift $ Remote.registerDriverRC (makeDriverRCReq state.data.vehicle_registration_number state.data.rcImageID state.data.dateOfRegistration)
          case registerDriverRCResp of
            Right (DriverRCResp resp) -> do
              void $ lift $ lift $ toggleLoader false
              setValueToLocalStore DOCUMENT_UPLOAD_TIME (getCurrentUTC "")
              applicationSubmittedFlow "StatusScreen"
            Left errorPayload -> do
              void $ lift $ lift $ toggleLoader false
              modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { data { dateOfRegistration = Just ""}}
              if errorPayload.code == 400 || (errorPayload.code == 500 && (decodeErrorCode errorPayload.response.errorMessage) == "UNPROCESSABLE_ENTITY") then do
                let correspondingErrorMessage =  Remote.getCorrespondingErrorMessage $ decodeErrorCode errorPayload.response.errorMessage
                modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { props {errorVisibility = true}, data {errorMessage = correspondingErrorMessage}}
                addVehicleDetailsflow
                else do
                  _ <- pure $ toast $ getString SOMETHING_WENT_WRONG
                  addVehicleDetailsflow

    VALIDATE_IMAGE_API_CALL state -> do
      void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_VALIDATING_THE_IMAGE)
      void $ lift $ lift $ toggleLoader true
      validateImageResp <- lift $ lift $ Remote.validateImage (makeValidateImageReq state.data.rc_base64 "VehicleRegistrationCertificate")
      case validateImageResp of
       Right (ValidateImageRes resp) -> do
        void $ lift $ lift $ toggleLoader false
        modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> state)
        modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> addVehicleDetailsScreen {data { rcImageID = resp.imageId}})
        addVehicleDetailsflow
       Left errorPayload -> do
        void $ lift $ lift $ toggleLoader false
        if errorPayload.code == 429 && (decodeErrorCode errorPayload.response.errorMessage) == "IMAGE_VALIDATION_EXCEED_LIMIT" then do
          modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> addVehicleDetailsScreen {props { limitExceedModal = true}})
          addVehicleDetailsflow
          else if errorPayload.code == 400 || (errorPayload.code == 500 && (decodeErrorCode errorPayload.response.errorMessage) == "UNPROCESSABLE_ENTITY") then do
            let correspondingErrorMessage =  Remote.getCorrespondingErrorMessage $ decodeErrorCode errorPayload.response.errorMessage
            modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { props {errorVisibility = true}, data {errorMessage = correspondingErrorMessage , rcImageID = "IMAGE_NOT_VALIDATED" }}
            addVehicleDetailsflow
            else do
              _ <- pure $ toast $ getString SOMETHING_WENT_WRONG
              modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { data {rcImageID = "IMAGE_NOT_VALIDATED" }}
              addVehicleDetailsflow

    LOGOUT_USER -> do
      (LogOutRes resp) <- Remote.logOutBT LogOutReq
      deleteValueFromLocalStore REGISTERATION_TOKEN
      deleteValueFromLocalStore LANGUAGE_KEY
      deleteValueFromLocalStore VERSION_NAME
      deleteValueFromLocalStore BASE_URL
      deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
      deleteValueFromLocalStore IS_DRIVER_ENABLED
      deleteValueFromLocalStore BUNDLE_VERSION
      deleteValueFromLocalStore DRIVER_ID
      deleteValueFromLocalStore SET_ALTERNATE_TIME
      pure $ factoryResetApp ""
      _ <- pure $ firebaseLogEvent "logout"
      loginFlow

    REFER_API_CALL state -> do
      void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      referDriverResponse <- lift $ lift $ Remote.referDriver (makeReferDriverReq state.data.referral_mobile_number)
      case referDriverResponse of
        Right (ReferDriverResp resp) -> do
          void $ lift $ lift $ toggleLoader false
          modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> state)
          addVehicleDetailsflow
        Left errorPayload -> do
          void $ lift $ lift $ toggleLoader false
          _ <- pure $ toast $ if decodeErrorMessage errorPayload.response.errorMessage /= "" then decodeErrorMessage errorPayload.response.errorMessage else getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
          addVehicleDetailsflow
    APPLICATION_STATUS_SCREEN -> applicationSubmittedFlow "StatusScreen"
    ONBOARDING_FLOW -> onBoardingFlow

applicationSubmittedFlow :: String -> FlowBT String Unit
applicationSubmittedFlow screenType = do
  lift $ lift $ doAff do liftEffect hideSplash
  action <- UI.applicationStatus screenType
  setValueToLocalStore TEST_FLOW_FOR_REGISTRATOION "COMPLETED"
  case action of
    GO_TO_HOME_FROM_APPLICATION_STATUS -> permissionsScreenFlow
    GO_TO_UPLOAD_DL_SCREEN -> do
      let (GlobalState defaultEpassState') = defaultGlobalState
      modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen -> defaultEpassState'.uploadDrivingLicenseScreen)
      modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> defaultEpassState'.addVehicleDetailsScreen)
      uploadDrivingLicenseFlow
    GO_TO_VEHICLE_DETAIL_SCREEN -> do
      let (GlobalState defaultEpassState') = defaultGlobalState
      modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen -> defaultEpassState'.uploadDrivingLicenseScreen)
      modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> defaultEpassState'.addVehicleDetailsScreen)
      addVehicleDetailsflow
    VALIDATE_NUMBER state -> do
      getAlternateMobileResp <- lift $ lift $ Remote.validateAlternateNumber (makeValidateAlternateNumberRequest (state.data.mobileNumber))
      case  getAlternateMobileResp of
            Right (DriverAlternateNumberResp resp) -> do
              modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen -> state{props{enterMobileNumberView = true,enterOtp=true,buttonVisibilty=false,isValidOtp=false}})
              applicationSubmittedFlow screenType
            Left errorPayload -> do
              if (errorPayload.code == 400 && (decodeErrorCode errorPayload.response.errorMessage) == "INVALID_REQUEST") then do
                modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen -> state {props{enterMobileNumberView = true, isAlternateMobileNumberExists = true,enterOtp = false}})
              else do
                pure $ toast $ getString SOMETHING_WENT_WRONG
                modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen -> state {props{enterMobileNumberView = false,enterOtp=false,buttonVisibilty=false}})
              applicationSubmittedFlow screenType
    VALIDATE_OTP state -> do
      getVerifyAlternateMobileOtpResp <- lift $ lift $ Remote.verifyAlternateNumberOTP (makeVerifyAlternateNumberOtpRequest (state.data.otpValue))
      case getVerifyAlternateMobileOtpResp of
        Right (DriverAlternateNumberOtpResp resp) -> do
          pure $ toast $ getString NUMBER_ADDED_SUCCESSFULLY
          modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen ->  state{props{enterOtp = false , enterMobileNumberView = false , alternateNumberAdded = true}})
          applicationSubmittedFlow screenType
        Left errorPayload -> do
            if (errorPayload.code == 400 && (decodeErrorCode errorPayload.response.errorMessage) == "INVALID_AUTH_DATA") then do
                modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen -> state{props{isValidOtp = true}})
                applicationSubmittedFlow screenType
            else if (errorPayload.code == 429 && (decodeErrorCode errorPayload.response.errorMessage == "HITS_LIMIT_EXCEED")) then do
              pure $ toast $ getString OTP_LIMIT_EXCEEDED
              (setValueToLocalStore INVALID_OTP_TIME (getCurrentUTC ""))
              modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen -> state{props{enterOtp = false , enterMobileNumberView = false}})
              applicationSubmittedFlow screenType
            else do
              pure $ toast (decodeErrorCode errorPayload.response.errorMessage)
              applicationSubmittedFlow screenType
    RESEND_OTP_TO_ALTERNATE_NUMBER state-> do
      let number =  state.data.mobileNumber
      getAlternateMobileResendOtpResp <- lift $ lift $ Remote.resendAlternateNumberOTP (makeResendAlternateNumberOtpRequest (number))
      case getAlternateMobileResendOtpResp of
            Right (AlternateNumberResendOTPResp resp) -> do
                pure $ toast (getString OTP_RESENT)
                applicationSubmittedFlow screenType
            Left errorPayload -> do
              if (errorPayload.code == 400 &&(decodeErrorCode errorPayload.response.errorMessage) == "AUTH_BLOCKED") then do
                  pure $ toast (getString OTP_RESEND_LIMIT_EXCEEDED)
                  applicationSubmittedFlow screenType
              else do
                  pure $ toast (decodeErrorCode errorPayload.response.errorMessage)
                  applicationSubmittedFlow screenType
    LOGOUT_ACCOUT -> do
      (LogOutRes resp) <- Remote.logOutBT LogOutReq
      deleteValueFromLocalStore REGISTERATION_TOKEN
      deleteValueFromLocalStore LANGUAGE_KEY
      deleteValueFromLocalStore VERSION_NAME
      deleteValueFromLocalStore BASE_URL
      deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
      deleteValueFromLocalStore IS_DRIVER_ENABLED
      deleteValueFromLocalStore BUNDLE_VERSION
      deleteValueFromLocalStore DRIVER_ID
      deleteValueFromLocalStore SET_ALTERNATE_TIME
      pure $ factoryResetApp ""
      _ <- pure $ firebaseLogEvent "logout"
      loginFlow

driverProfileFlow :: FlowBT String Unit
driverProfileFlow = do
  _ <- pure $ delay $ Milliseconds 1.0
  _ <- pure $ printLog "Registration token" (getValueToLocalStore REGISTERATION_TOKEN)
  (GetRidesHistoryResp rideHistoryResponse) <- Remote.getRideHistoryReqBT "1" "0" "false"
  case (head rideHistoryResponse.list) of
    Nothing -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen { props {isNoRides = true}})
    Just (RidesInfo response) -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen { data {customerName = fromMaybe "" (response.riderName),
        source = (decodeAddress response.fromLocation false),
        destination = (decodeAddress response.toLocation false),
        date = (convertUTCtoISC (response.createdAt) "D MMM"),
        time = (convertUTCtoISC (response.createdAt )"h:mm A"),
        tripId = response.shortRideId,
        coveredDistance = (parseFloat (toNumber (fromMaybe 0 response.chargeableDistance) / 1000.0) 2 ),
        durationOfTrip = "",  -- TODO:: {once beckend send durationOfTrip} response.durationOfTrip
        rating = fromMaybe 0 response.rideRating,
        fare = fromMaybe response.estimatedBaseFare response.computedFare}})
  action <- UI.driverProfileScreen
  case action of
    GO_TO_HOME_FROM_PROFILE -> homeScreenFlow
    GO_TO_REFERRAL_SCREEN_FROM_DRIVER_PROFILE_SCREEN -> referralScreenFlow
    DRIVER_DETAILS_SCREEN -> driverDetailsFlow
    VEHICLE_DETAILS_SCREEN -> vehicleDetailsFlow
    ABOUT_US_SCREEN -> aboutUsFlow
    SELECT_LANGUAGE_SCREEN -> selectLanguageFlow
    ON_BOARDING_FLOW -> onBoardingFlow
    GO_TO_LOGOUT -> do
      (LogOutRes resp) <- Remote.logOutBT LogOutReq
      removeChatService ""
      lift $ lift $ liftFlow $ stopLocationPollingAPI
      deleteValueFromLocalStore REGISTERATION_TOKEN
      deleteValueFromLocalStore LANGUAGE_KEY
      deleteValueFromLocalStore VERSION_NAME
      deleteValueFromLocalStore BASE_URL
      deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
      deleteValueFromLocalStore IS_DRIVER_ENABLED
      deleteValueFromLocalStore DRIVER_STATUS
      deleteValueFromLocalStore BUNDLE_VERSION
      deleteValueFromLocalStore DRIVER_ID
      deleteValueFromLocalStore SET_ALTERNATE_TIME
      pure $ factoryResetApp ""
      _ <- pure $ firebaseLogEvent "logout"
      loginFlow
    HELP_AND_SUPPORT_SCREEN -> helpAndSupportFlow
    GO_TO_DRIVER_HISTORY_SCREEN -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{offsetValue = 0, currentTab = "COMPLETED"})
      myRidesScreenFlow
    GO_TO_EDIT_BANK_DETAIL_SCREEN -> editBankDetailsFlow
    NOTIFICATIONS_SCREEN -> notificationFlow
    GO_TO_BOOKING_OPTIONS_SCREEN state-> do 
      modifyScreenState $ BookingOptionsScreenType (\bookingOptions -> bookingOptions{data{vehicleType = state.data.driverVehicleType, vehicleNumber = state.data.vehicleRegNumber, vehicleName = state.data.vehicleModelName, vehicleCapacity = state.data.capacity, downgradeOptions = ((downgradeOptionsConfig state.data.vehicleSelected) <$> state.data.downgradeOptions)}})
      bookingOptionsFlow

driverDetailsFlow :: FlowBT String Unit
driverDetailsFlow = do
  action <- UI.driverDetailsScreen
  case action of
    DRIVER_ALTERNATE_CALL_API updatedState -> do
      let number =  if (updatedState.props.isEditAlternateMobile) then updatedState.data.driverEditAlternateMobile else updatedState.data.driverAlternateMobile
      getAlternateMobileResp <- lift $ lift $ Remote.validateAlternateNumber (makeValidateAlternateNumberRequest (fromMaybe "" (number)))
      case  getAlternateMobileResp of
            Right (DriverAlternateNumberResp resp) -> do
                  modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen ->updatedState)
                  driverDetailsFlow
            Left errorPayload -> do
               let alternateNumber = if updatedState.props.isEditAlternateMobile then updatedState.data.driverAlternateMobile else Nothing
               if (errorPayload.code == 400 && ((decodeErrorCode errorPayload.response.errorMessage) == "INVALID_REQUEST")) then do
                modifyScreenState $ DriverDetailsScreenStateType $ \driverDetailsScreen -> updatedState { props {numberExistError = true,keyboardModalType = MOBILE__NUMBER }}
                driverDetailsFlow
               else do
                  pure $ toast $ (getString ALTERNATE_NUMBER_CANNOT_BE_ADDED)
                  modifyScreenState $ DriverDetailsScreenStateType $ \driverDetailsScreen -> updatedState { data {driverAlternateMobile = alternateNumber} , props {keyboardModalType = NONE,isEditAlternateMobile = false,checkAlternateNumber = (alternateNumber == Nothing)}}
                  driverDetailsFlow




    RESEND_ALTERNATE_OTP updatedState -> do
      let number =  if (updatedState.props.isEditAlternateMobile) then updatedState.data.driverEditAlternateMobile else updatedState.data.driverAlternateMobile
      getAlternateMobileResendOtpResp <- lift $ lift $ Remote.resendAlternateNumberOTP (makeResendAlternateNumberOtpRequest (fromMaybe "" (number)))
      case getAlternateMobileResendOtpResp of
            Right (AlternateNumberResendOTPResp resp) -> do
                pure $ toast (getString OTP_RESENT)
                modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> updatedState)
                driverDetailsFlow
            Left errorPayload -> do
              if (errorPayload.code == 400 &&(decodeErrorCode errorPayload.response.errorMessage) == "AUTH_BLOCKED") then do
                  modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> updatedState)
                  pure $ toast (getString OTP_RESEND_LIMIT_EXCEEDED)
                  driverDetailsFlow
              else do
                  pure $ toast (decodeErrorMessage errorPayload.response.errorMessage)
                  modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> updatedState {data{ driverAlternateMobile = (if(updatedState.props.isEditAlternateMobile) then updatedState.data.driverAlternateMobile else Nothing), driverEditAlternateMobile = Nothing} , props{  otpIncorrect = false ,otpAttemptsExceeded = false ,keyboardModalType = NONE , alternateMobileOtp = "",checkAlternateNumber =(updatedState.props.isEditAlternateMobile == false) }})
                  driverDetailsFlow


    VERIFY_OTP state -> do
       let toast_value = if (state.props.isEditAlternateMobile == false) then (getString NUMBER_ADDED_SUCCESSFULLY) else (getString NUMBER_EDITED_SUCCESSFULLY)
           finalAlternateMobileNumber = state.data.driverEditAlternateMobile
       getVerifyAlternateMobileOtpResp <- lift $ lift $ Remote.verifyAlternateNumberOTP (makeVerifyAlternateNumberOtpRequest (state.props.alternateMobileOtp))
       case getVerifyAlternateMobileOtpResp of
         Right (DriverAlternateNumberOtpResp resp) -> do
              pure $ toast (toast_value)
              modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state {data {driverAlternateMobile = finalAlternateMobileNumber , driverEditAlternateMobile = Nothing} , props {otpIncorrect = false ,otpAttemptsExceeded = false ,keyboardModalType = NONE , alternateMobileOtp = "",checkAlternateNumber = false,isEditAlternateMobile = false}})
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data = homeScreen.data {  driverAlternateMobile = finalAlternateMobileNumber  }})
              modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {data = driverProfileScreen.data { driverAlternateNumber = finalAlternateMobileNumber}})
              driverDetailsFlow
         Left errorPayload -> do
            if (errorPayload.code == 400 && (decodeErrorCode errorPayload.response.errorMessage) == "INVALID_AUTH_DATA") then do
               if (state.data.otpLimit == 1)
               then do
                modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state { props {otpAttemptsExceeded = true, keyboardModalType = NONE}})
                setValueToLocalStore SET_ALTERNATE_TIME ((getCurrentUTC ""))
                driverDetailsFlow
               else do
                let otpExceeded = ((state.data.otpLimit - 1) <= 0)
                modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state {data  { otpLimit = state.data.otpLimit - 1 } , props {otpIncorrect = (if (otpExceeded) then false else true) ,otpAttemptsExceeded = otpExceeded,alternateMobileOtp = ""}})
                driverDetailsFlow
            else if (errorPayload.code == 429 && (decodeErrorCode errorPayload.response.errorMessage == "HITS_LIMIT_EXCEED"))
                then do
                  modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state { props {otpAttemptsExceeded = true, keyboardModalType = NONE}})
                  setValueToLocalStore SET_ALTERNATE_TIME ((getCurrentUTC ""))
                  driverDetailsFlow
            else do
                pure $ toast (decodeErrorMessage errorPayload.response.errorMessage)
                modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state {data{ driverAlternateMobile = (if(state.props.isEditAlternateMobile) then state.data.driverAlternateMobile else Nothing), driverEditAlternateMobile = Nothing} , props{  otpIncorrect = false ,otpAttemptsExceeded = false ,keyboardModalType = NONE , alternateMobileOtp = "",checkAlternateNumber =(state.props.isEditAlternateMobile == false) }})
                driverDetailsFlow


    ALTERNATE_NUMBER_REMOVE state -> do
       getAlternateMobileRemoveResp <- lift $ lift $ Remote.removeAlternateNumber (RemoveAlternateNumberRequest {} )
       case  getAlternateMobileRemoveResp of
          Right (RemoveAlternateNumberResp resp) -> do
                pure $ toast (getString NUMBER_REMOVED_SUCCESSFULLY)
                modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state { data {driverAlternateMobile = Nothing}, props  { checkAlternateNumber = true}})
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data = homeScreen.data {  driverAlternateMobile = Nothing  }})
                modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {data = driverProfileScreen.data { driverAlternateNumber = Nothing}})
                driverDetailsFlow
          Left errorPayload -> do
               _ <- pure $ toast $ (decodeErrorCode errorPayload.response.errorMessage)
               modifyScreenState $ DriverDetailsScreenStateType $ \driverDetailsScreen -> state
               driverDetailsFlow

    GO_TO_HOMESCREEN state -> do
       modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state {props {keyboardModalType = NONE}} )
       homeScreenFlow

  pure unit

vehicleDetailsFlow :: FlowBT String Unit
vehicleDetailsFlow = do
  action <- UI.vehicleDetailsScreen
  case action of
    UPDATE_VEHICLE_INFO  updatedState -> do
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT (makeUpdateDriverInfoReq "")
      vehicleDetailsFlow

aboutUsFlow :: FlowBT String Unit
aboutUsFlow = do
  action <- UI.aboutUsScreen
  case action of
    GO_TO_DRIVER_HOME_SCREEN -> homeScreenFlow
  pure unit

selectLanguageFlow :: FlowBT String Unit
selectLanguageFlow = do
  action <- UI.selectLanguageScreen
  case action of
    CHANGE_LANGUAGE -> do
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT (makeUpdateDriverLangChange "")
      driverProfileFlow

bookingOptionsFlow :: FlowBT String Unit
bookingOptionsFlow = do 
  action <- UI.bookingOptions
  case action of
    SELECT_CAB state -> do
      let toSedan = (filter (\item -> item.vehicleVariant == "SEDAN" && item.isSelected) state.data.downgradeOptions) !! 0
          toHatchBack = (filter (\item -> item.vehicleVariant == "HATCHBACK" && item.isSelected) state.data.downgradeOptions) !! 0
          toTaxi = (filter (\item -> item.vehicleVariant == "TAXI" && item.isSelected) state.data.downgradeOptions) !! 0
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT (spy "update Req" (makeUpdateBookingOptions (isJust toSedan) (isJust toHatchBack) (isJust toTaxi)))
      modifyScreenState $ BookingOptionsScreenType (\bookingOptions -> BookingOptionsScreenData.initData)
      driverProfileFlow
    GO_TO_PROFILE -> driverProfileFlow

helpAndSupportFlow :: FlowBT String Unit
helpAndSupportFlow = do
  action <- UI.helpAndSupportScreen
  case action of
    WRITE_TO_US_SCREEN -> writeToUsFlow
    TRIP_DETAILS_SCREEN updatedState -> do
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen { data {rider = updatedState.data.customerName,
        source = updatedState.data.source,
        destination = updatedState.data.destination,
        date = updatedState.data.date,
        time = updatedState.data.time,
        tripId = updatedState.data.tripId,
        distance = updatedState.data.coveredDistance,
        timeTaken = updatedState.data.durationOfTrip,
        totalAmount = (updatedState.data.fare)}})
      tripDetailsScreenFlow
    MY_RIDES_SCREEN -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{offsetValue = 0, currentTab = "COMPLETED"})
      myRidesScreenFlow
    REPORT_ISSUE -> writeToUsFlow

writeToUsFlow :: FlowBT String Unit
writeToUsFlow = do
  action <- UI.writeToUsScreen
  case action of
    GO_TO_HOME_SCREEN_FLOW -> homeScreenFlow

permissionsScreenFlow :: FlowBT String Unit
permissionsScreenFlow = do
  lift $ lift $ doAff do liftEffect hideSplash
  _ <- pure $ hideKeyboardOnNavigation true
  action <- UI.permissions
  case action of
    DRIVER_HOME_SCREEN -> do
      setValueToLocalStore TEST_FLOW_FOR_REGISTRATOION "COMPLETED"
      setValueToLocalStore TEST_FLOW_FOR_PERMISSIONS "COMPLETED"
      currentRideFlow

myRidesScreenFlow :: FlowBT String Unit
myRidesScreenFlow = do
  flow <- UI.rideHistory
  case flow of
    REFRESH state -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> state{offsetValue = 0})
      myRidesScreenFlow
    MY_RIDE state -> tripDetailsScreenFlow
    HOME_SCREEN -> homeScreenFlow
    PROFILE_SCREEN -> driverProfileFlow
    GO_TO_REFERRAL_SCREEN -> referralScreenFlow
    LOADER_OUTPUT state -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> state{offsetValue = state.offsetValue + 8})
      myRidesScreenFlow
    FILTER currTab-> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{currentTab = currTab})
      myRidesScreenFlow
    GO_TO_TRIP_DETAILS selectedCard -> do
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {data {
      tripId = selectedCard.id,
      date = selectedCard.date,
      time = selectedCard.time,
      source = selectedCard.source,
      destination = selectedCard.destination,
      totalAmount = selectedCard.total_amount,
      distance = selectedCard.rideDistance,
      status = selectedCard.status
      }})
      tripDetailsScreenFlow
    NOTIFICATION_FLOW -> notificationFlow

referralScreenFlow :: FlowBT String Unit
referralScreenFlow = do
  (GlobalState state) <- getState
  if isJust state.referralScreen.data.driverInfo.referralCode then do
    (GetPerformanceRes getPerformanceres) <- Remote.getPerformanceBT (GetPerformanceReq {} )
    modifyScreenState $ ReferralScreenStateType (\ referralScreen -> referralScreen { data { driverPerformance { referrals = getPerformanceres.referrals}}} )
    else pure unit
  act <- UI.referralScreen
  case act of
    GO_TO_HOME_SCREEN_FROM_REFERRAL_SCREEN -> homeScreenFlow
    GO_TO_RIDES_SCREEN_FROM_REFERRAL_SCREEN -> myRidesScreenFlow
    GO_TO_PROFILE_SCREEN_FROM_REFERRAL_SCREEN -> driverProfileFlow
    GO_TO_NOTIFICATION_SCREEN_FROM_REFERRAL_SCREEN -> notificationFlow
    GO_TO_FLOW_AND_COME_BACK updatedState-> do
      response <-  lift $ lift $ Remote.linkReferralCode ( makeLinkReferralCodeReq updatedState.data.referralCode updatedState.data.password)
      case response of
        Right resp -> do
          modifyScreenState $ ReferralScreenStateType (\ referralScreen -> referralScreen{ data { driverInfo {referralCode = Just updatedState.data.referralCode} } ,  props { stage = SuccessScreen , firstTime = true}})
          referralScreenFlow
        Left error -> do
          _ <- pure $ toast (decodeErrorMessage error.response.errorMessage)
          referralScreenFlow
      referralScreenFlow
    _ -> homeScreenFlow

tripDetailsScreenFlow :: FlowBT String Unit
tripDetailsScreenFlow = do
  flow <- UI.tripDetailsScreen
  case flow of
    ON_SUBMIT  -> pure unit
    GO_TO_HOME_SCREEN -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{offsetValue = 0, currentTab = "COMPLETED"})
      myRidesScreenFlow

currentRideFlow :: FlowBT String Unit
currentRideFlow = do
  setValueToLocalStore RIDE_STATUS_POLLING "False"
  let isRequestExpired = if (getValueToLocalNativeStore RIDE_REQUEST_TIME) == "__failed" then false
    else ceil ((toNumber (rideRequestPollingData.duration - (getExpiryTime (getValueToLocalNativeStore RIDE_REQUEST_TIME) true)) * 1000.0)/rideRequestPollingData.delay) > 0
  if isLocalStageOn RideRequested && (getValueToLocalNativeStore IS_RIDE_ACTIVE) == "false" && isRequestExpired then
    homeScreenFlow
    else pure unit
  (GetRidesHistoryResp activeRideResponse) <- Remote.getRideHistoryReqBT "1" "0" "true"
  _ <- pure $ spy "activeRideResponse" activeRideResponse
  if not (null activeRideResponse.list) then do
    case (activeRideResponse.list !! 0 ) of
      Just ride -> do
        (GlobalState allState) <- getState
        let state = allState.homeScreen
            activeRide = (activeRideDetail state ride)
            stage = (if activeRide.status == NEW then (if state.props.currentStage == ChatWithCustomer then ChatWithCustomer else RideAccepted) else RideStarted)
        setValueToLocalNativeStore IS_RIDE_ACTIVE  "true"
        _ <- updateStage $ HomeScreenStage stage
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data{ activeRide = activeRide}, props{ silentPopUpView = false, goOfflineModal = false }})
        homeScreenFlow
      Nothing -> do
        setValueToLocalNativeStore IS_RIDE_ACTIVE  "false"
        _ <- updateStage $ HomeScreenStage HomeScreen
        homeScreenFlow
    else do
      setValueToLocalNativeStore IS_RIDE_ACTIVE  "false"
      _ <- updateStage $ HomeScreenStage HomeScreen
      homeScreenFlow

getDriverStatus :: String -> DriverStatus
getDriverStatus dummy = do
  case getValueToLocalNativeStore DRIVER_STATUS_N of
    "Online" -> Online
    "Offline" -> Offline
    "Silent" -> Silent
    _ -> Online

getDriverStatusFromMode :: String -> DriverStatus
getDriverStatusFromMode mode = do
  case mode of
    "ONLINE" -> Online
    "OFFLINE" -> Offline
    "SILENT" -> Silent
    _ -> Online

updateDriverStatus :: Boolean -> DriverStatus
updateDriverStatus status = do
  if status && getValueToLocalNativeStore DRIVER_STATUS_N == "Silent" then Silent
    else if status then Online
      else Offline

homeScreenFlow :: FlowBT String Unit
homeScreenFlow = do
  _ <- pure $ printLog "HOME_SCREEN_FLOW" "."
  _ <- pure $ delay $ Milliseconds 1.0
  _ <- pure $ printLog "Registration token" (getValueToLocalStore REGISTERATION_TOKEN)
  _ <- pure $ firebaseUserID (getValueToLocalStore DRIVER_ID)
  if (getValueToLocalNativeStore IS_RIDE_ACTIVE) == "true" && not (isLocalStageOn RideAccepted) && not (isLocalStageOn RideStarted) && not (isLocalStageOn ChatWithCustomer) then
    currentRideFlow
  else pure unit
  getDriverInfoResp <- Remote.getDriverInfoBT (GetDriverInfoReq { })
  let (GetDriverInfoResp getDriverInfoResp) = getDriverInfoResp
  let (Vehicle linkedVehicle) = (fromMaybe dummyVehicleObject getDriverInfoResp.linkedVehicle)
  setValueToLocalStore USER_NAME getDriverInfoResp.firstName
  _ <- pure $ spy "response for mode zxc" getDriverInfoResp
  case getDriverInfoResp.mode of
    Just currentMode -> case currentMode of
                          "OFFLINE" -> do
                              lift $ lift $ liftFlow $ stopLocationPollingAPI
                              setDriverStatusInLocal "false" (show $ getDriverStatusFromMode currentMode)
                              _ <- pure $ spy "setting offline zxc " (getValueToLocalStore DRIVER_STATUS_N)
                              pure unit
                          _ ->        do
                                      setDriverStatusInLocal "true" (show $ getDriverStatusFromMode currentMode)
                                      lift $ lift $ liftFlow $ startLocationPollingAPI
                                      _ <- pure $ spy "setting ONLINE/SILENT zxc " (getValueToLocalStore DRIVER_STATUS_N)
                                      pure unit

    Nothing -> do
                setDriverStatusInLocal (show getDriverInfoResp.active) (show $ updateDriverStatus (getDriverInfoResp.active))
                _ <- pure $ spy "setting ONLINE/SILENT zxc when mode is nothing" (getValueToLocalStore DRIVER_STATUS_N)
                lift $ lift $ liftFlow $ if getDriverInfoResp.active then  startLocationPollingAPI else stopLocationPollingAPI
                (DriverActiveInactiveResp resp) <- Remote.driverActiveInactiveBT (if any( _ == (updateDriverStatus getDriverInfoResp.active))[Online, Silent] then "true" else "false") $ toUpper $ show (updateDriverStatus getDriverInfoResp.active)
                _ <- pure $ spy "Checking respons for mode " resp
                pure unit


  _  <- pure $ spy "DRIVERRRR___STATUs" (getValueToLocalNativeStore DRIVER_STATUS)
  _  <- pure $ spy "DRIVERRRR___STATUS LOCAL" (getValueToLocalStore DRIVER_STATUS)
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {statusOnline = if (isJust getDriverInfoResp.mode) then
                                                                                                (any( _ == (updateDriverStatus (getDriverInfoResp.active)))[Online, Silent])
                                                                                             else getDriverInfoResp.active
                                                                            , driverStatusSet = getDriverStatus "" }
                                                                      , data{vehicleType = linkedVehicle.variant, driverAlternateMobile =getDriverInfoResp.alternateNumber}})
  
  modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {data {driverName = getDriverInfoResp.firstName, driverVehicleType = linkedVehicle.variant, driverRating = getDriverInfoResp.rating, capacity = fromMaybe 2 linkedVehicle.capacity, downgradeOptions = getDowngradeOptions linkedVehicle.variant, vehicleSelected = getDowngradeOptionsSelected (GetDriverInfoResp getDriverInfoResp)}})
  modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> driverDetailsScreen { data {driverAlternateMobile =getDriverInfoResp.alternateNumber}})
  modifyScreenState $ ReferralScreenStateType (\ referralScreen -> referralScreen{ data { driverInfo  {  driverName = getDriverInfoResp.firstName, driverMobile = getDriverInfoResp.mobileNumber,  vehicleRegNumber = linkedVehicle.registrationNo , referralCode = getDriverInfoResp.referralCode }}})
  let currdate = getcurrentdate ""
  (DriverProfileStatsResp resp) <- Remote.getDriverProfileStatsBT (DriverProfileStatsReq currdate)
  lift $ lift $ doAff do liftEffect hideSplash
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data{totalRidesOfDay = resp.totalRidesOfDay, totalEarningsOfDay = resp.totalEarningsOfDay}})
  void $ lift $ lift $ toggleLoader false
  isGpsEnabled <- lift $ lift $ liftFlow $ isLocationEnabled unit
  if not isGpsEnabled then noInternetScreenFlow "LOCATION_DISABLED" else pure unit
  action <- UI.homeScreen
  case action of
    GO_TO_PROFILE_SCREEN -> driverProfileFlow
    GO_TO_RIDES_SCREEN -> do
      _ <- pure $ printLog "HOME_SCREEN_FLOW GO_TO_RIDES_SCREEN" "."
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{offsetValue = 0 , currentTab = "COMPLETED"})
      myRidesScreenFlow
    GO_TO_REFERRAL_SCREEN_FROM_HOME_SCREEN -> referralScreenFlow
    DRIVER_AVAILABILITY_STATUS status -> do
      _ <- setValueToLocalStore DRIVER_STATUS if any( _ == status)[Online, Silent] then "true" else "false" --(show status)
      _ <- setValueToLocalStore DRIVER_STATUS_N $ show status
      void $ lift $ lift $ loaderText (getString PLEASE_WAIT) if any( _ == status)[Online, Silent] then (getString SETTING_YOU_ONLINE) else (getString SETTING_YOU_OFFLINE)
      void $ lift $ lift $ toggleLoader true
      (DriverActiveInactiveResp resp) <- Remote.driverActiveInactiveBT (if any( _ == status)[Online, Silent] then "true" else "false") $ toUpper $ show status
      _ <- setValueToLocalStore RIDE_T_FREQUENCY (if status == Online then "20000" else "30000")
      _ <- setValueToLocalStore DRIVER_MIN_DISPLACEMENT (if any( _ == status)[Online, Silent] then "8.0" else "25.0")
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {statusOnline = (if any( _ == status)[Online, Silent] then true else false), driverStatusSet = status}})
      _ <- pure $ spy "zxc updateActivity " status
      homeScreenFlow

    GO_TO_START_RIDE {id, otp , lat, lon} -> do
      _ <- pure $ printLog "HOME_SCREEN_FLOW GO_TO_START_RIDE" "."
      _ <- pure $ printLog "Lat in Floww: " lat
      _ <- pure $ printLog "Lon in Floww: " lon
      void $ lift $ lift $ loaderText (getString START_RIDE) ""
      void $ lift $ lift $ toggleLoader true
      startRideResp <- lift $ lift $ Remote.startRide id (Remote.makeStartRideReq otp (fromMaybe 0.0 (Number.fromString lat)) (fromMaybe 0.0 (Number.fromString lon))) -- driver's lat long during starting ride
      case startRideResp of
        Right startRideResp -> do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props {enterOtpModal = false, showDottedRoute = true}, data{ route = [], activeRide{status = INPROGRESS}}})
          void $ lift $ lift $ toggleLoader false
          _ <- updateStage $ HomeScreenStage RideStarted
          currentRideFlow
        Left errorPayload -> do
          let errResp = errorPayload.response
          let codeMessage = decodeErrorCode errResp.errorMessage
          if ( errorPayload.code == 400 && codeMessage == "INCORRECT_OTP") then do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {otpIncorrect = true, enterOtpModal = true, otpAttemptsExceeded = false, rideOtp = ""} })
              void $ lift $ lift $ toggleLoader false
            else if ( errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {otpAttemptsExceeded = true, enterOtpModal = true, rideOtp = ""} })
              void $ lift $ lift $ toggleLoader false
              else pure $ toast (getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER)
          homeScreenFlow
    GO_TO_START_ZONE_RIDE {otp, lat, lon} -> do
      void $ lift $ lift $ loaderText (getString PLEASE_WAIT) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      startZoneRideResp <- lift $ lift $ Remote.otpRide "" (Remote.makeOTPRideReq otp (fromMaybe 0.0 (Number.fromString lat)) (fromMaybe 0.0 (Number.fromString lon))) -- driver's lat long during starting ride      
      case startZoneRideResp of
        Right startZoneRideResp -> do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props {enterOtpModal = false, showDottedRoute = true}, data{ route = [], activeRide{status = INPROGRESS}}})
          void $ lift $ lift $ toggleLoader false
          void $ updateStage $ HomeScreenStage RideStarted
          currentRideFlow
        Left errorPayload -> do
          let errResp = errorPayload.response
          let codeMessage = decodeErrorCode errResp.errorMessage
          if ( errorPayload.code == 400 && (codeMessage == "BOOKING_NOT_FOUND_FOR_SPECIAL_ZONE_OTP")) then do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {otpIncorrect = true, enterOtpModal = true, otpAttemptsExceeded = false, rideOtp = ""} })
            else if ( errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {otpAttemptsExceeded = true, enterOtpModal = true, rideOtp = ""} })
              else pure $ toast (getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER)
          void $ lift $ lift $ toggleLoader false
          homeScreenFlow
    GO_TO_END_RIDE {id, lat, lon} -> do
      _ <- pure $ printLog "HOME_SCREEN_FLOW GO_TO_END_RIDE" "."
      void $ lift $ lift $ loaderText (getString END_RIDE) ""
      void $ lift $ lift $ toggleLoader true
      endRideResp <- Remote.endRide id (Remote.makeEndRideReq (fromMaybe 0.0 (Number.fromString lat)) (fromMaybe 0.0 (Number.fromString lon)))-- driver's  lat long during ending ride
      _ <- pure $ removeAllPolylines ""
      _ <- pure $ setValueToLocalNativeStore IS_RIDE_ACTIVE  "false"
      _ <- pure $ setValueToLocalStore DRIVER_STATUS_N "Online"
      _ <- pure $ setValueToLocalNativeStore DRIVER_STATUS_N "Online"
      (DriverActiveInactiveResp resp) <- Remote.driverActiveInactiveBT "true" $ toUpper $ show Online
      _ <- pure $ firebaseLogEvent "ny_user_ride_completed"
      (GetRidesHistoryResp rideHistoryResponse) <- Remote.getRideHistoryReqBT "1" "0" "false"
      case (head rideHistoryResponse.list) of
        Nothing -> pure unit
        Just (RidesInfo response) -> do
          _ <- pure $ printLog "ride history response" response
          _ <- pure $ printLog "fromLocation lat" (response.fromLocation ^._lat)
          modifyScreenState $ RideDetailScreenStateType (\rideDetailScreen -> rideDetailScreen { data {customerName = fromMaybe "" (response.riderName),
            sourceAddress {
              place = (decodeAddress response.fromLocation false),
              lat = (response.fromLocation ^._lat),
              lon = (response.fromLocation ^._lon)
            },
            destAddress {
              place = (decodeAddress response.toLocation false),
              lat = (response.toLocation ^._lat),
              lon = (response.toLocation ^._lon)
            },
            rideStartTime = convertUTCtoISC (fromMaybe " " response.tripStartTime) "h:mm a",
            rideEndTime = convertUTCtoISC (fromMaybe " " response.tripEndTime) "h:mm a",
            bookingDateAndTime = convertUTCtoISC response.createdAt  "DD/MM/yyyy  hh:mm a",
            totalAmount = fromMaybe response.estimatedBaseFare response.computedFare}})
      void $ lift $ lift $ toggleLoader false
      _ <- updateStage $ HomeScreenStage RideCompleted
      rideDetailFlow
    GO_TO_CANCEL_RIDE {id, info , reason} -> do
      _ <- pure $ printLog "HOME_SCREEN_FLOW GO_TO_CANCEL_RIDE" "."
      cancelRideResp <- Remote.cancelRide id (Remote.makeCancelRideReq info reason)
      _ <- pure $ removeAllPolylines ""
      _ <- pure $ setValueToLocalStore DRIVER_STATUS_N "Online"
      _ <- pure $ setValueToLocalNativeStore DRIVER_STATUS_N "Online"
      (DriverActiveInactiveResp resp) <- Remote.driverActiveInactiveBT "true" $ toUpper $ show Online
      removeChatService ""
      _ <- updateStage $ HomeScreenStage HomeScreen
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props { chatcallbackInitiated = false}})
      homeScreenFlow
    FCM_NOTIFICATION notificationType -> do
      _ <- pure $ removeAllPolylines ""
      case notificationType of
        "CANCELLED_PRODUCT" -> do
          _ <- pure $ setValueToLocalStore DRIVER_STATUS_N "Online"
          _ <- pure $ setValueToLocalNativeStore DRIVER_STATUS_N "Online"
          (DriverActiveInactiveResp resp) <- Remote.driverActiveInactiveBT "true" $ toUpper $ show Online
          removeChatService ""
          _ <- updateStage $ HomeScreenStage HomeScreen
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props { chatcallbackInitiated = false}})
          homeScreenFlow
        "DRIVER_ASSIGNMENT" -> currentRideFlow
        "RIDE_REQUESTED"    -> do
          _ <- updateStage $ HomeScreenStage RideRequested
          homeScreenFlow
        _                   -> homeScreenFlow
    REFRESH_HOME_SCREEN_FLOW -> do
      _ <- pure $ removeAllPolylines ""
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props {rideActionModal = false, cancelRideModalShow = false, enterOtpModal = false, routeVisible = false, refreshAnimation = false}})
      homeScreenFlow
    RELOAD state -> do
      _ <- pure $ printLog "HOME_SCREEN_FLOW RELOAD" state
      homeScreenFlow
    NOTIFY_CUSTOMER state -> do
      resp <- Remote.driverArrivedBT (state.data.activeRide.id) (DriverArrivedReq {
        "lat" : state.data.currentDriverLat
      , "lon" : state.data.currentDriverLon
      })
      _ <- pure $ firebaseLogEvent "i_have_arrived_clicked"
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{activeRide{notifiedCustomer = true}}, props{currentStage = ChatWithCustomer}})
      homeScreenFlow
    UPDATE_ROUTE state -> do
      _ <- pure $ printLog "HOME_SCREEN_FLOW UPDATE_ROUTE" state
      let srcLat = if state.props.currentStage == RideAccepted then state.data.currentDriverLat else state.data.activeRide.src_lat
          srcLon = if state.props.currentStage == RideAccepted then state.data.currentDriverLon else state.data.activeRide.src_lon
          destLat = if state.props.currentStage == RideAccepted then state.data.activeRide.src_lat else state.data.activeRide.dest_lat
          destLon = if state.props.currentStage == RideAccepted then state.data.activeRide.src_lon else state.data.activeRide.dest_lon
          source = if state.props.currentStage == RideAccepted then "" else state.data.activeRide.source
          destination = if state.props.currentStage == RideAccepted then state.data.activeRide.source else state.data.activeRide.destination
          routeType = if state.props.currentStage == RideAccepted then "pickup" else "trip"
      if state.props.showDottedRoute then do
        let coors = (walkCoordinate srcLon srcLat destLon destLat)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { routeVisible = true } })
        _ <- pure $ removeAllPolylines ""
        _ <- lift $ lift $ doAff do liftEffect $ drawRoute coors "DOT" "#323643" false "ny_ic_src_marker" "ny_ic_dest_marker" 9 "NORMAL" source destination
        homeScreenFlow
        else if not null state.data.route then do
          let shortRoute = (state.data.route !! 0)
          case shortRoute of
            Just (Route route) -> do
              let coor = walkCoordinates route.points
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { routeVisible = true } })
              _ <- pure $ removeMarker "ic_vehicle_side"
              _ <- pure $ removeAllPolylines ""
              _ <- lift $ lift $ doAff do liftEffect $ drawRoute coor "LineString" "#323643" true "ny_ic_src_marker" "ny_ic_dest_marker" 9 "NORMAL" source destination
              pure unit
            Nothing -> pure unit
          homeScreenFlow
          else do
            GetRouteResp routeApiResponse <- Remote.getRouteBT (makeGetRouteReq srcLat srcLon destLat destLon) routeType
            _ <- pure $ printLog "route Api response inside UPDATE_ROUTE" routeApiResponse
            _ <- pure $ printLog "state inside UPDATE_ROUTE" state
            let shortRoute = (routeApiResponse !! 0)
            case shortRoute of
              Just (Route route) -> do
                let coor = walkCoordinates route.points
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { activeRide { actualRideDistance = if state.props.currentStage == RideStarted then (toNumber route.distance) else state.data.activeRide.actualRideDistance , duration = route.duration } , route = routeApiResponse}, props { routeVisible = true } })
                _ <- lift $ lift $ doAff do liftEffect $ removeMarker "ny_ic_auto"
                _ <- pure $ removeAllPolylines ""
                _ <- lift $ lift $ doAff do liftEffect $ drawRoute coor "ic_vehicle_side" "#323643" true "ny_ic_src_marker" "ny_ic_dest_marker" 9 "NORMAL" source destination
                pure unit
              Nothing -> pure unit
            homeScreenFlow
    UPDATE_STAGE stage -> do
      _ <- updateStage $ HomeScreenStage stage
      homeScreenFlow
    GO_TO_NOTIFICATIONS -> notificationFlow
    ADD_ALTERNATE_HOME -> do
      modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> driverDetailsScreen { data { driverAlternateMobile = Nothing }, props  { isEditAlternateMobile = false, keyboardModalType = MOBILE__NUMBER}})
      driverDetailsFlow
  pure unit

constructLatLong :: String -> String -> Location
constructLatLong lat lng =
  { lat: fromMaybe 0.0 (Number.fromString lat)
  , lon : fromMaybe 0.0 (Number.fromString lng)
  , place : ""
  }

updateCustomerMarker :: Location -> Effect Unit
updateCustomerMarker loc = pure unit

rideDetailFlow :: FlowBT String Unit
rideDetailFlow = do
  action <- UI.rideDetail
  case action of
    GO_TO_HOME_FROM_RIDE_DETAIL -> do
      _ <- updateStage $ HomeScreenStage HomeScreen
      currentRideFlow
    SHOW_ROUTE_IN_RIDE_DETAIL -> do
      void $ lift $ lift $ toggleLoader false
      modifyScreenState $ RideDetailScreenStateType (\rideDetail -> rideDetail { props { cashCollectedButton = true } } )
      rideDetailFlow

editBankDetailsFlow :: FlowBT String Unit
editBankDetailsFlow = do
  action <- UI.editBankDetailsScreen
  pure unit

editAadhaarDetailsFlow :: FlowBT String Unit
editAadhaarDetailsFlow = do
  action <- UI.editAadhaarDetailsScreen
  pure unit

noInternetScreenFlow :: String -> FlowBT String Unit
noInternetScreenFlow triggertype = do
  action <- UI.noInternetScreen triggertype
  internetCondition <- lift $ lift $ liftFlow $ isInternetAvailable unit
  case action of
    REFRESH_INTERNET -> case ((ifNotRegistered unit) || (getValueToLocalStore IS_DRIVER_ENABLED == "false")) of
                        true -> pure unit
                        false ->  currentRideFlow
    TURN_ON_GPS -> if not internetCondition then noInternetScreenFlow "INTERNET_ACTION"
                    else do
                      (DriverActiveInactiveResp resp) <- Remote.driverActiveInactiveBT "true" $ toUpper $ show Online
                      currentRideFlow
    CHECK_INTERNET -> case ((ifNotRegistered unit) || (getValueToLocalStore IS_DRIVER_ENABLED == "false")) of
                      true  -> pure unit
                      false -> do
                        permissionsGiven <- checkAll3Permissions
                        if permissionsGiven
                          then currentRideFlow
                          else permissionsScreenFlow

checkAll3Permissions :: FlowBT String Boolean
checkAll3Permissions = do
  isLocationPermission <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
  isOverlayPermission <- lift $ lift $ liftFlow $ isOverlayPermissionEnabled unit
  isBatteryUsagePermission <- lift $ lift $ liftFlow $ isBatteryPermissionEnabled unit
  pure $ isLocationPermission && isOverlayPermission && isBatteryUsagePermission && getValueToLocalStore TEST_FLOW_FOR_PERMISSIONS == "COMPLETED"

popUpScreenFlow :: AllocationData -> FlowBT String Unit
popUpScreenFlow entityPayload = do
  _ <- pure $ printLog "popupscreen entity payload" entityPayload
  let availableRide = (transformAllocationData entityPayload)
  modifyScreenState $ PopUpScreenStateType (\popUpScreen -> popUpScreen{ data { availableRides = availableRide } })
  action <- UI.popUpScreen
  case action of
    POPUP_REQUEST_RIDE id extraFare -> do
      _ <- pure $ printLog "calling request ride api" "."
      if extraFare > 0.0 then do
        (OfferRideResp resp) <- Remote.offerRideBT (makeOfferRideReq id (Just extraFare))
        pure unit
        else do
          (OfferRideResp resp) <- Remote.offerRideBT (makeOfferRideReq id Nothing)
          pure unit
      homeScreenFlow
    _ -> do
      _ <- pure $ printLog "no action matched" "."
      homeScreenFlow

driverRideRatingFlow :: FlowBT String Unit
driverRideRatingFlow = do
  action <- UI.driverRideRatingScreen
  case action of
    SendCustomerFeedBack updatedState -> do
      --TODO // API CALL for feedback
      homeScreenFlow
    CloseScreen -> homeScreenFlow

notificationFlow :: FlowBT String Unit
notificationFlow = do
  screenAction <- UI.notifications
  case screenAction of
    REFRESH_SCREEN state -> do
      modifyScreenState $ NotificationsScreenStateType (\notificationScreen -> state{offsetValue = 0})
      notificationFlow
    LOAD_NOTIFICATIONS state -> do
      modifyScreenState $ NotificationsScreenStateType (\notificationScreen -> state{offsetValue = (length state.notificationList)})
      notificationFlow
    GO_HOME_SCREEN -> homeScreenFlow
    GO_REFERRAL_SCREEN -> referralScreenFlow
    GO_RIDE_HISTORY_SCREEN -> myRidesScreenFlow
    GO_PROFILE_SCREEN -> driverProfileFlow

removeChatService :: String -> FlowBT String Unit
removeChatService _ = do
  _ <- lift $ lift $ liftFlow $ stopChatListenerService
  _ <- pure $ setValueToLocalNativeStore READ_MESSAGES "0"
  pure unit

setDriverStatusInLocal :: String -> String -> FlowBT String Unit
setDriverStatusInLocal status mode = do
                                    setValueToLocalStore DRIVER_STATUS status
                                    setValueToLocalNativeStore DRIVER_STATUS status
                                    setValueToLocalStore DRIVER_STATUS_N mode
                                    setValueToLocalNativeStore DRIVER_STATUS_N mode
                                    pure unit
