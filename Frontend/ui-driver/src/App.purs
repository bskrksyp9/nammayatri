{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Types.App where

import Control.Transformers.Back.Trans (BackT)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Free (Free)
import Presto.Core.Types.Language.Flow (FlowWrapper)
import Screens.Types (AboutUsScreenState, ActiveRide, AddVehicleDetailsScreenState, AppUpdatePopUpScreenState, ApplicationStatusScreenState, BankDetailScreenState, CategoryListType, ChooseLanguageScreenState, DriverDetailsScreenState, DriverProfileScreenState, DriverRideRatingScreenState, DriverStatus, EditAadhaarDetailsScreenState, EditBankDetailsScreenState, EnterMobileNumberScreenState, EnterOTPScreenState, HelpAndSupportScreenState, HomeScreenState, IndividualRideCardState, NoInternetScreenState, NotificationsScreenState, PermissionsScreenState, PopUpScreenState, ReferralScreenState, RegistrationScreenState, ReportIssueChatScreenState, RideDetailScreenState, RideHistoryScreenState, RideSelectionScreenState, SelectLanguageScreenState, SplashScreenState, TripDetailsScreenState, UploadAdhaarScreenState, UploadDrivingLicenseState, VehicleDetailsScreenState, WriteToUsScreenState)
import Screens.ChooseLanguageScreen.ScreenData as ChooseLanguageScreenData
import Screens.EnterMobileNumberScreen.ScreenData as EnterMobileNumberScreenData
import Screens.EnterOTPScreen.ScreenData as  EnterOTPScreenData
import Screens.AddVehicleDetailsScreen.ScreenData as AddVehicleDetailsScreenData
import Screens.UploadDrivingLicenseScreen.ScreenData as UploadDrivingLicenseScreenData
import Screens.RegistrationScreen.ScreenData as RegistrationScreenData
import Screens.UploadAdhaarScreen.ScreenData as UploadAdhaarScreenData
import Screens.ApplicationStatusScreen.ScreenData as ApplicationStatusScreenData
import Screens.TripDetailsScreen.ScreenData as TripDetailsScreenData
import Screens.RideHistoryScreen.ScreenData as RideHistoryScreenData
import Screens.RideSelectionScreen.ScreenData as RideSelectionScreenData
import Screens.BankDetailScreen.ScreenData as BankDetailScreenData
import Screens.DriverProfileScreen.ScreenData as DriverProfileScreenData
import Screens.DriverDetailsScreen.ScreenData as DriverDetailsScreenData
import Screens.VehicleDetailsScreen.ScreenData as VehicleDetailsScreenData
import Screens.AboutUsScreen.ScreenData as AboutUsScreenData
import Screens.SelectLanguageScreen.ScreenData as SelectLanguageScreenData
import Screens.HelpAndSupportScreen.ScreenData as HelpAndSupportScreenData
import Screens.WriteToUsScreen.ScreenData as WriteToUsScreenData
import Screens.PermissionsScreen.ScreenData as PermissionsScreenData
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.RideDetailScreen.ScreenData as RideDetailScreenData
import Screens.ReportIssueChatScreen.ScreenData as ReportIssueChatScreenData
import Screens.EditBankDetailsScreen.ScreenData as EditBankDetailsScreenData
import Screens.EditAadhaarDetailsScreen.ScreenData as EditAadhaarDetailsScreenData
import Screens.PopUpScreen.ScreenData as PopUpScreenData
import Screens.DriverRideRatingScreen.ScreenData as DriverRideRatingScreenData
import Screens.NotificationsScreen.ScreenData as NotificationsScreenData
import Screens.ReferralScreen.ScreenData as ReferralScreenData
import Screens.Types (HomeScreenStage(..))

type FlowBT e a = BackT (ExceptT e (Free (FlowWrapper GlobalState))) a

newtype GlobalState = GlobalState { 
    splashScreen :: SplashScreenState
  , chooseLanguageScreen :: ChooseLanguageScreenState
  , driverProfileScreen :: DriverProfileScreenState
  , applicationStatusScreen :: ApplicationStatusScreenState
  , mobileNumberScreen :: EnterMobileNumberScreenState
  , enterOTPScreen :: EnterOTPScreenState
  , uploadDrivingLicenseScreen :: UploadDrivingLicenseState
  , registrationScreen :: RegistrationScreenState
  , uploadAdhaarScreen :: UploadAdhaarScreenState
  , addVehicleDetailsScreen :: AddVehicleDetailsScreenState
  , tripDetailsScreen :: TripDetailsScreenState
  , rideHistoryScreen :: RideHistoryScreenState
  , rideSelectionScreen :: RideSelectionScreenState
  , reportIssueChatScreen :: ReportIssueChatScreenState
  , bankDetailsScreen :: BankDetailScreenState
  , driverDetailsScreen :: DriverDetailsScreenState
  , vehicleDetailsScreen :: VehicleDetailsScreenState
  , aboutUsScreen :: AboutUsScreenState
  , selectedLanguageScreen :: SelectLanguageScreenState
  , helpAndSupportScreen :: HelpAndSupportScreenState
  , writeToUsScreen :: WriteToUsScreenState
  , permissionsScreen :: PermissionsScreenState
  , homeScreen :: HomeScreenState
  , rideDetailScreen :: RideDetailScreenState
  , editBankDetailsScreen :: EditBankDetailsScreenState
  , editAadhaarDetailsScreen :: EditAadhaarDetailsScreenState
  , noInternetScreen :: NoInternetScreenState
  , popUpScreen :: PopUpScreenState
  , driverRideRatingScreen :: DriverRideRatingScreenState
  , appUpdatePopUpScreen :: AppUpdatePopUpScreenState
  , notificationScreen :: NotificationsScreenState
  , referralScreen :: ReferralScreenState
  }

defaultGlobalState :: GlobalState
defaultGlobalState = GlobalState{
  splashScreen : {data : { message : "WeDontTalkAnymore"}}
, chooseLanguageScreen : ChooseLanguageScreenData.initData
, driverProfileScreen : DriverProfileScreenData.initData
, applicationStatusScreen : ApplicationStatusScreenData.initData
, mobileNumberScreen : EnterMobileNumberScreenData.initData
, enterOTPScreen : EnterOTPScreenData.initData
, uploadDrivingLicenseScreen : UploadDrivingLicenseScreenData.initData
, registrationScreen: RegistrationScreenData.initData
, uploadAdhaarScreen : UploadAdhaarScreenData.initData
, addVehicleDetailsScreen : AddVehicleDetailsScreenData.initData
, tripDetailsScreen : TripDetailsScreenData.initData
, rideHistoryScreen : RideHistoryScreenData.initData
, rideSelectionScreen : RideSelectionScreenData.initData
, reportIssueChatScreen : ReportIssueChatScreenData.initData
, bankDetailsScreen : BankDetailScreenData.initData
, driverDetailsScreen : DriverDetailsScreenData.initData
, vehicleDetailsScreen : VehicleDetailsScreenData.initData
, aboutUsScreen : AboutUsScreenData.initData
, selectedLanguageScreen : SelectLanguageScreenData.initData
, helpAndSupportScreen : HelpAndSupportScreenData.initData
, writeToUsScreen : WriteToUsScreenData.initData
, permissionsScreen : PermissionsScreenData.initData
, homeScreen : HomeScreenData.initData
, rideDetailScreen : RideDetailScreenData.initData
, editBankDetailsScreen : EditBankDetailsScreenData.initData
, editAadhaarDetailsScreen : EditAadhaarDetailsScreenData.initData
, noInternetScreen : {}
, popUpScreen : PopUpScreenData.initData
, driverRideRatingScreen : DriverRideRatingScreenData.initData
, appUpdatePopUpScreen : {version : 1}
, notificationScreen : NotificationsScreenData.initData
, referralScreen : ReferralScreenData.initData
}

data ScreenType =
   SplashScreenStateType (SplashScreenState -> SplashScreenState)
  | ChooseLanguageScreenStateType (ChooseLanguageScreenState -> ChooseLanguageScreenState)
  | DriverProfileScreenStateType (DriverProfileScreenState -> DriverProfileScreenState)
  | ApplicationStatusScreenType (ApplicationStatusScreenState -> ApplicationStatusScreenState)
  | EnterMobileNumberScreenType (EnterMobileNumberScreenState -> EnterMobileNumberScreenState)
  | EnterOTPScreenType (EnterOTPScreenState -> EnterOTPScreenState)
  | UploadDrivingLicenseScreenStateType (UploadDrivingLicenseState -> UploadDrivingLicenseState)
  | RegisterScreenStateType (RegistrationScreenState -> RegistrationScreenState)
  | UploadAdhaarScreenStateType (UploadAdhaarScreenState -> UploadAdhaarScreenState)
  | AddVehicleDetailsScreenStateType (AddVehicleDetailsScreenState -> AddVehicleDetailsScreenState)
  | DriverDetailsScreenStateType (DriverDetailsScreenState -> DriverDetailsScreenState)
  | VehicleDetailsScreenStateType (VehicleDetailsScreenState -> VehicleDetailsScreenState)
  | AboutUsScreenStateType (AboutUsScreenState -> AboutUsScreenState)
  | SelectLanguageScreenStateType (SelectLanguageScreenState -> SelectLanguageScreenState)
  | HelpAndSupportScreenStateType (HelpAndSupportScreenState -> HelpAndSupportScreenState)
  | WriteToUsScreenStateType (WriteToUsScreenState -> WriteToUsScreenState)
  | BankDetailScreenStateType (BankDetailScreenState -> BankDetailScreenState)
  | HomeScreenStateType (HomeScreenState -> HomeScreenState)
  | RideDetailScreenStateType (RideDetailScreenState -> RideDetailScreenState)
  | RideHistoryScreenStateType (RideHistoryScreenState -> RideHistoryScreenState)
  | RideSelectionScreenStateType (RideSelectionScreenState -> RideSelectionScreenState)
  | ReportIssueChatScreenStateType (ReportIssueChatScreenState -> ReportIssueChatScreenState)
  | PermissionsScreenStateType (PermissionsScreenState -> PermissionsScreenState)
  | EditBankDetailsScreenStateType (EditBankDetailsScreenState -> EditBankDetailsScreenState)
  | EditAadhaarDetailsScreenStateType (EditAadhaarDetailsScreenState -> EditAadhaarDetailsScreenState)
  | TripDetailsScreenStateType (TripDetailsScreenState -> TripDetailsScreenState)
  | PopUpScreenStateType (PopUpScreenState -> PopUpScreenState)
  | DriverRideRatingScreenStateType (DriverRideRatingScreenState -> DriverRideRatingScreenState)
  | NotificationsScreenStateType (NotificationsScreenState -> NotificationsScreenState)
  | ReferralScreenStateType (ReferralScreenState -> ReferralScreenState)

data ScreenStage = HomeScreenStage HomeScreenStage

data MY_RIDES_SCREEN_OUTPUT = MY_RIDE RideHistoryScreenState 
                            | HOME_SCREEN 
                            | PROFILE_SCREEN 
                            | GO_TO_REFERRAL_SCREEN
                            | REFRESH RideHistoryScreenState 
                            | LOADER_OUTPUT RideHistoryScreenState 
                            | FILTER String 
                            | GO_TO_TRIP_DETAILS IndividualRideCardState
                            | NOTIFICATION_FLOW

data REFERRAL_SCREEN_OUTPUT = GO_TO_HOME_SCREEN_FROM_REFERRAL_SCREEN
                            | GO_TO_RIDES_SCREEN_FROM_REFERRAL_SCREEN
                            | GO_TO_PROFILE_SCREEN_FROM_REFERRAL_SCREEN
                            | GO_TO_NOTIFICATION_SCREEN_FROM_REFERRAL_SCREEN
                            | GO_TO_FLOW_AND_COME_BACK ReferralScreenState

data RIDES_SELECTION_SCREEN_OUTPUT = REFRESH_RIDES RideSelectionScreenState
                                   | LOADER_RIDES_OUTPUT RideSelectionScreenState
                                   | SELECT_RIDE RideSelectionScreenState

data DRIVER_PROFILE_SCREEN_OUTPUT = DRIVER_DETAILS_SCREEN 
                                    | VEHICLE_DETAILS_SCREEN 
                                    | ABOUT_US_SCREEN 
                                    | GO_TO_LOGOUT 
                                    | SELECT_LANGUAGE_SCREEN 
                                    | HELP_AND_SUPPORT_SCREEN 
                                    | GO_TO_HOME_FROM_PROFILE 
                                    | GO_TO_DRIVER_HISTORY_SCREEN 
                                    | GO_TO_EDIT_BANK_DETAIL_SCREEN
                                    | ON_BOARDING_FLOW
                                    | NOTIFICATIONS_SCREEN
                                    | GO_TO_REFERRAL_SCREEN_FROM_DRIVER_PROFILE_SCREEN


data DRIVER_DETAILS_SCREEN_OUTPUT = VERIFY_OTP DriverDetailsScreenState
                                  | DRIVER_ALTERNATE_CALL_API DriverDetailsScreenState
                                  | RESEND_ALTERNATE_OTP DriverDetailsScreenState
                                  | ALTERNATE_NUMBER_REMOVE DriverDetailsScreenState
                                  | GO_TO_HOMESCREEN DriverDetailsScreenState


data VEHICLE_DETAILS_SCREEN_OUTPUT = UPDATE_VEHICLE_INFO VehicleDetailsScreenState
data ABOUT_US_SCREEN_OUTPUT = GO_TO_DRIVER_HOME_SCREEN
data SELECT_LANGUAGE_SCREEN_OUTPUT = CHANGE_LANGUAGE
data HELP_AND_SUPPORT_SCREEN_OUTPUT = WRITE_TO_US_SCREEN 
                                    | REPORT_ISSUE_CHAT_SCREEN CategoryListType
                                    | RIDE_SELECTION_SCREEN CategoryListType
                                    | REMOVE_ISSUE_SCREEN String HelpAndSupportScreenState 
                                    | RESOLVED_ISSUE_SCREEN HelpAndSupportScreenState 
                                    | ON_GOING_ISSUE_SCREEN HelpAndSupportScreenState 
                                    | ISSUE_LIST_GO_BACK_SCREEN HelpAndSupportScreenState 
                                 

data WRITE_TO_US_SCREEN_OUTPUT = GO_TO_HOME_SCREEN_FLOW
data REGISTRATION_SCREENOUTPUT = UPLOAD_DRIVER_LICENSE

data UPLOAD_DRIVER_LICENSE_SCREENOUTPUT = ADD_VEHICLE_DETAILS_SCREEN UploadDrivingLicenseState | VALIDATE_IMAGE_API UploadDrivingLicenseState | GOTO_VEHICLE_DETAILS_SCREEN | LOGOUT_ACCOUNT | GOTO_ONBOARDING_FLOW

data UPLOAD_ADHAAR_CARD_SCREENOUTPUT = GO_TO_ADD_BANK_DETAILS

data BANK_DETAILS_SCREENOUTPUT = GO_TO_ADD_VEHICLE_DETAILS

data ADD_VEHICLE_DETAILS_SCREENOUTPUT = GO_TO_APPLICATION_SCREEN AddVehicleDetailsScreenState | VALIDATE_IMAGE_API_CALL AddVehicleDetailsScreenState | REFER_API_CALL AddVehicleDetailsScreenState | APPLICATION_STATUS_SCREEN | LOGOUT_USER | ONBOARDING_FLOW

data TRIP_DETAILS_SCREEN_OUTPUT = ON_SUBMIT | GO_TO_HOME_SCREEN | OPEN_HELP_AND_SUPPORT

data PERMISSIONS_SCREEN_OUTPUT = DRIVER_HOME_SCREEN

data HOME_SCREENOUTPUT = GO_TO_PROFILE_SCREEN
                          | GO_TO_RIDES_SCREEN 
                          | GO_TO_REFERRAL_SCREEN_FROM_HOME_SCREEN
                          | GO_TO_HELP_AND_SUPPORT_SCREEN
                          | GO_TO_START_RIDE {id :: String, otp :: String, lat :: String, lon :: String}
                          | GO_TO_CANCEL_RIDE {id :: String, info :: String , reason :: String} 
                          | GO_TO_END_RIDE {id :: String, lat :: String , lon :: String } 
                          | DRIVER_AVAILABILITY_STATUS DriverStatus
                          | REFRESH_HOME_SCREEN_FLOW
                          | RELOAD HomeScreenState
                          | UPDATE_ROUTE HomeScreenState
                          | FCM_NOTIFICATION String
                          | NOTIFY_CUSTOMER HomeScreenState
                          | UPDATE_STAGE HomeScreenStage
                          | GO_TO_NOTIFICATIONS
                          | ADD_ALTERNATE_HOME 
                          
data REPORT_ISSUE_CHAT_SCREEN_OUTPUT = GO_TO_HELP_AND_SUPPORT | SUBMIT_ISSUE ReportIssueChatScreenState | CALL_CUSTOMER ReportIssueChatScreenState

data RIDE_DETAIL_SCREENOUTPUT = GO_TO_HOME_FROM_RIDE_DETAIL | SHOW_ROUTE_IN_RIDE_DETAIL
data APPLICATION_STATUS_SCREENOUTPUT = GO_TO_HOME_FROM_APPLICATION_STATUS
                                      | LOGOUT_ACCOUT
                                      | GO_TO_UPLOAD_DL_SCREEN
                                      | GO_TO_VEHICLE_DETAIL_SCREEN
                                      | VALIDATE_NUMBER ApplicationStatusScreenState
                                      | VALIDATE_OTP ApplicationStatusScreenState
                                      | RESEND_OTP_TO_ALTERNATE_NUMBER ApplicationStatusScreenState
data EDIT_BANK_DETAILS_SCREEN_OUTPUT = EDIT_BANK_DETAILS
data EDIT_AADHAAR_DETAILS_SCREEN_OUTPUT = EDIT_AADHAAR_DETAILS
data ENTER_MOBILE_NUMBER_SCREEN_OUTPUT = GO_TO_ENTER_OTP EnterMobileNumberScreenState
data ENTER_OTP_SCREEN_OUTPUT = RETRY EnterOTPScreenState | DRIVER_INFO_API_CALL EnterOTPScreenState
data NO_INTERNET_SCREEN_OUTPUT = REFRESH_INTERNET | TURN_ON_GPS | CHECK_INTERNET
data POPUP_SCREEN_OUTPUT = POPUP_REQUEST_RIDE String Number
data DRIVER_RIDE_RATING_SCREEN_OUTPUT = CloseScreen | SendCustomerFeedBack DriverRideRatingScreenState
data NOTIFICATIONS_SCREEN_OUTPUT = REFRESH_SCREEN NotificationsScreenState 
                                    | LOAD_NOTIFICATIONS NotificationsScreenState 
                                    | GO_HOME_SCREEN 
                                    | GO_REFERRAL_SCREEN 
                                    | GO_RIDE_HISTORY_SCREEN 
                                    | GO_PROFILE_SCREEN