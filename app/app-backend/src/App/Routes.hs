{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Routes where

import qualified API.Auth as Auth
import qualified API.Beckn as Beckn
import qualified API.MetroBeckn as MetroBeckn
import qualified API.UI.Booking as Booking
import qualified API.UI.Cancel as Cancel
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.Confirm as Confirm
import qualified API.UI.Feedback as Feedback
import qualified API.UI.Profile as Profile
import qualified API.UI.Quote as Quote
import qualified API.UI.Registration as Registration
import qualified API.UI.Route as Route
import qualified API.UI.SavedReqLocation as SavedReqLocation
import qualified API.UI.Search as Search
import qualified API.UI.Select as Select
import qualified API.UI.Serviceability as Serviceability
import qualified API.UI.Support as Support
import qualified App.Routes.Dashboard as Dashboard
import App.Types
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.App
import Beckn.Types.Id
import Data.OpenApi (Info (..), OpenApi (..))
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude
import qualified Product.Call as Call
import qualified Product.CustomerSupport as CS
import qualified Product.Ride as Ride
import qualified Product.Services.GoogleMaps as GoogleMapsFlow
import Servant hiding (throwError)
import Servant.OpenApi
import qualified Types.API.Call as API
import qualified Types.API.CustomerSupport as CustomerSupport
import qualified Types.API.Ride as RideAPI
import Utils.Auth (TokenAuth)

type AppAPI =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  UIAPI
    :<|> Beckn.API
    :<|> MetroBeckn.API
    :<|> Auth.API
    :<|> Dashboard.API

type UIAPI =
  "v2"
    :> ( Get '[JSON] Text
           :<|> Registration.API
           :<|> Profile.API
           :<|> Search.API
           :<|> Select.API
           :<|> Quote.API
           :<|> Confirm.API
           :<|> Booking.API
           :<|> Cancel.API
           :<|> RideAPI
           :<|> DeprecatedCallAPIs
           :<|> CallAPIs
           :<|> Support.API
           :<|> Route.API
           :<|> Serviceability.API
           :<|> Feedback.API
           :<|> CustomerSupportAPI
           :<|> GoogleMapsProxyAPI
           :<|> CancellationReason.API
           :<|> SavedReqLocation.API
       )

appAPI :: Proxy AppAPI
appAPI = Proxy

appServer :: FlowServer AppAPI
appServer =
  mainServer
    :<|> writeSwaggerJSONFlow

mainServer :: FlowServer MainAPI
mainServer =
  uiAPI
    :<|> Beckn.handler
    :<|> MetroBeckn.handler
    :<|> Auth.handler
    :<|> Dashboard.handler

uiAPI :: FlowServer UIAPI
uiAPI =
  pure "App is UP"
    :<|> Registration.handler
    :<|> Profile.handler
    :<|> Search.handler
    :<|> Select.handler
    :<|> Quote.handler
    :<|> Confirm.handler
    :<|> Booking.handler
    :<|> Cancel.handler
    :<|> rideFlow
    :<|> deprecatedCallFlow
    :<|> callFlow
    :<|> Support.handler
    :<|> Route.handler
    :<|> Serviceability.handler
    :<|> Feedback.handler
    :<|> customerSupportFlow
    :<|> googleMapsProxyFlow
    :<|> CancellationReason.handler
    :<|> SavedReqLocation.handler

type RideAPI =
  "ride"
    :> Capture "rideId" (Id SRide.Ride)
    :> "driver"
    :> "location"
    :> TokenAuth
    :> Post '[JSON] RideAPI.GetDriverLocRes

rideFlow :: FlowServer RideAPI
rideFlow =
  Ride.getDriverLoc

-------- Initiate a call (Exotel) APIs --------
type DeprecatedCallAPIs =
  "ride"
    :> Capture "rideId" (Id SRide.Ride)
    :> "call"
    :> ( "driver"
           :> TokenAuth
           :> Post '[JSON] API.CallRes
           :<|> "statusCallback"
           :> ReqBody '[JSON] API.CallCallbackReq
           :> Post '[JSON] API.CallCallbackRes
           :<|> Capture "callId" (Id SCS.CallStatus)
           :> "status"
           :> TokenAuth
           :> Get '[JSON] API.GetCallStatusRes
       )

deprecatedCallFlow :: FlowServer DeprecatedCallAPIs
deprecatedCallFlow rideId =
  Call.initiateCallToDriver rideId
    :<|> Call.callStatusCallback rideId
    :<|> Call.getCallStatus rideId

-------- Direct call (Exotel) APIs
type CallAPIs =
  "exotel"
    :> "call"
    :> ( "driver"
           :> "number"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "CallFrom" Text
           :> MandatoryQueryParam "CallTo" Text
           :> MandatoryQueryParam "CallStatus" Text
           :> Get '[JSON] API.MobileNumberResp
           :<|> "statusCallback"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "DialCallStatus" Text
           :> MandatoryQueryParam "RecordingUrl" Text
           :> QueryParam "Legs[0][OnCallDuration]" Int
           :> Get '[JSON] API.CallCallbackRes
       )

callFlow :: FlowServer CallAPIs
callFlow =
  Call.getDriverMobileNumber
    :<|> Call.directCallStatusCallback

-- Customer Support Flow --

type CustomerSupportAPI =
  "customerSupport"
    :> ( "login"
           :> ReqBody '[JSON] CustomerSupport.LoginReq
           :> Post '[JSON] CustomerSupport.LoginRes
           :<|> "logout"
             :> TokenAuth
             :> Post '[JSON] CustomerSupport.LogoutRes
           :<|> "orders"
             :> TokenAuth
             :> QueryParam "id" Text
             :> QueryParam "phone" Text
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] [CustomerSupport.OrderResp]
       )

customerSupportFlow :: FlowServer CustomerSupportAPI
customerSupportFlow =
  CS.login
    :<|> CS.logout
    :<|> CS.listOrder

type GoogleMapsProxyAPI =
  "googleMaps"
    :> ( "autoComplete"
           :> TokenAuth
           :> MandatoryQueryParam "input" Text
           :> MandatoryQueryParam "location" Text -- Passing it as <latitude>,<longitude>
           :> MandatoryQueryParam "radius" Integer
           :> MandatoryQueryParam "language" Text
           :> Get '[JSON] GoogleMaps.SearchLocationResp
           :<|> "placeDetails"
             :> TokenAuth
             :> MandatoryQueryParam "place_id" Text
             :> Get '[JSON] GoogleMaps.PlaceDetailsResp
           :<|> "getPlaceName"
             :> TokenAuth
             :> MandatoryQueryParam "latlng" Text -- Passing it as <latitude>,<longitude>
             :> QueryParam "language" Text
             :> Get '[JSON] GoogleMaps.GetPlaceNameResp
       )

googleMapsProxyFlow :: FlowServer GoogleMapsProxyAPI
googleMapsProxyFlow =
  GoogleMapsFlow.autoComplete
    :<|> GoogleMapsFlow.placeDetails
    :<|> GoogleMapsFlow.getPlaceName

type SwaggerAPI = "swagger" :> Get '[JSON] OpenApi

swagger :: OpenApi
swagger = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Yatri",
            _infoVersion = "1.0"
          }
    }

writeSwaggerJSONFlow :: FlowServer SwaggerAPI
writeSwaggerJSONFlow = return swagger
