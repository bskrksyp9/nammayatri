module App.Routes where

import qualified API.Beckn.Handler as Beckn
import qualified API.UI.Booking.Handler as Booking
import qualified API.UI.Call.Handler as Call
import qualified API.UI.CancellationReason.Handler as CancellationReason
import qualified API.UI.Driver.Handler as Driver
import qualified API.UI.FarePolicy.Handler as FarePolicy
import qualified API.UI.GoogleMaps.Handler as GoogleMaps
import qualified API.UI.Location.Handler as Location
import qualified API.UI.Registration.Handler as Registration
import qualified API.UI.Ride.Handler as Ride
import qualified API.UI.Route.Handler as Route
import qualified API.UI.TranspAdmin.Handler as TranspAdmin
import qualified API.UI.Transporter.Handler as Transporter
import qualified API.UI.Vehicle.Handler as Vehicle
import App.Types
import Data.OpenApi
import EulerHS.Prelude
import Servant
import Servant.OpenApi

type TransportAPI =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  "v2" :> UIAPI
    :<|> "v1" :> OrgBecknAPI

type UIAPI =
  HealthCheckAPI
    :<|> Registration.API
    :<|> TranspAdmin.API
    :<|> Driver.API
    :<|> Vehicle.API
    :<|> Transporter.API
    :<|> Booking.API
    :<|> FarePolicy.API
    :<|> Location.API
    :<|> Call.API
    :<|> Route.API
    :<|> Ride.API
    :<|> CancellationReason.API
    :<|> GoogleMaps.API

transporterAPI :: Proxy TransportAPI
transporterAPI = Proxy

uiServer :: FlowServer UIAPI
uiServer =
  pure "App is UP"
    :<|> Registration.handler
    :<|> TranspAdmin.handler
    :<|> Driver.handler
    :<|> Vehicle.handler
    :<|> Transporter.handler
    :<|> Booking.handler
    :<|> FarePolicy.handler
    :<|> Location.handler
    :<|> Call.handler
    :<|> Route.handler
    :<|> Ride.handler
    :<|> CancellationReason.handler
    :<|> GoogleMaps.handler

mainServer :: FlowServer MainAPI
mainServer =
  uiServer
    :<|> orgBecknApiFlow

transporterServer :: FlowServer TransportAPI
transporterServer =
  mainServer
    :<|> writeSwaggerJSONFlow

-- location flow over
type OrgBecknAPI = Beckn.API

orgBecknApiFlow :: FlowServer OrgBecknAPI
orgBecknApiFlow = Beckn.handler

type HealthCheckAPI = Get '[JSON] Text

type SwaggerAPI = "swagger" :> Get '[JSON] OpenApi

swagger :: OpenApi
swagger = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Yatri Partner",
            _infoVersion = "1.0"
          }
    }

writeSwaggerJSONFlow :: FlowServer SwaggerAPI
writeSwaggerJSONFlow = return swagger
