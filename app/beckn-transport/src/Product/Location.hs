module Product.Location where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Types.Error.API
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as QPI
import qualified Beckn.Types.Storage.RegistrationToken as SR
import EulerHS.Prelude hiding (id, state)
import qualified Storage.Queries.Location as Location
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import Types.API.Location as Location
import Utils.Common

updateLocation :: SR.RegistrationToken -> UpdateLocationReq -> FlowHandler UpdateLocationRes
updateLocation SR.RegistrationToken {..} req = withFlowHandlerAPI $ do
  person <- Person.findPersonById $ Id entityId
  driver <- if person.role == Person.DRIVER then return person else throwError AccessDenied
  locationId <-
    driver.locationId
      & fromMaybeM (PersonFieldNotPresent "location_id")
  logTagInfo "driverLocationUpdate" (entityId <> " " <> show req.lat <> "," <> show req.long)
  Location.updateGpsCoord locationId (req.lat) (req.long)
  return $ UpdateLocationRes "ACK"

getLocation :: Id QPI.ProductInstance -> FlowHandler GetLocationRes
getLocation piId = withFlowHandlerAPI $ do
  orderProductInstance <- ProductInstance.findByParentIdType piId Case.RIDEORDER
  driver <-
    orderProductInstance.personId & fromMaybeM (PIFieldNotPresent "person")
      >>= Person.findPersonById
  currLocation <-
    driver.locationId
      & fromMaybeM (PersonFieldNotPresent "location_id")
      >>= Location.findLocationById
      >>= fromMaybeM LocationNotFound
  lat <- currLocation.lat & fromMaybeM (LocationFieldNotPresent "lat")
  long <- currLocation.long & fromMaybeM (LocationFieldNotPresent "long")
  return $ GetLocationRes {location = Location.LocationInfo lat long}

getRoute' :: Double -> Double -> Double -> Double -> Flow (Maybe MapSearch.Route)
getRoute' fromLat fromLon toLat toLon = MapSearch.getRouteMb getRouteRequest
  where
    getRouteRequest = do
      let from = MapSearch.LatLong $ MapSearch.PointXY fromLat fromLon
      let to = MapSearch.LatLong $ MapSearch.PointXY toLat toLon
      MapSearch.Request
        { waypoints = [from, to],
          mode = Just MapSearch.CAR,
          departureTime = Nothing,
          arrivalTime = Nothing,
          calcPoints = Just True
        }

getRoute :: SR.RegistrationToken -> Location.Request -> FlowHandler Location.Response
getRoute _ Location.Request {..} =
  withFlowHandlerAPI $ MapSearch.getRoute getRouteRequest
  where
    mapToMapPoint (Location.LatLong lat long) = MapSearch.LatLong $ MapSearch.PointXY lat long
    getRouteRequest =
      MapSearch.Request
        { waypoints = mapToMapPoint <$> waypoints,
          mode = mode <|> Just MapSearch.CAR,
          departureTime = Nothing,
          arrivalTime = Nothing,
          calcPoints
        }

calculateDistance :: Location.Location -> Location.Location -> Flow (Maybe Float)
calculateDistance source destination = do
  routeRequest <- mkRouteRequest
  fmap MapSearch.distanceInM <$> MapSearch.getRouteMb routeRequest
  where
    mkRouteRequest = do
      sourceLat <- source.lat & fromMaybeM (LocationFieldNotPresent "source.lat")
      sourceLng <- source.long & fromMaybeM (LocationFieldNotPresent "source.long")
      destinationLat <- destination.lat & fromMaybeM (LocationFieldNotPresent "dest.lat")
      destinationLng <- destination.long & fromMaybeM (LocationFieldNotPresent "dest.long")
      let sourceMapPoint = mkMapPoint sourceLat sourceLng
      let destinationMapPoint = mkMapPoint destinationLat destinationLng
      pure $
        MapSearch.Request
          { waypoints = [sourceMapPoint, destinationMapPoint],
            mode = Just MapSearch.CAR,
            departureTime = Nothing,
            arrivalTime = Nothing,
            calcPoints = Just False
          }
    mkMapPoint lat lng = MapSearch.LatLong $ MapSearch.PointXY lat lng
