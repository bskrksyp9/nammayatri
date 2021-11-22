{-# LANGUAGE TypeApplications #-}

module Product.Location where

import App.Types
import qualified Beckn.Product.MapSearch as MapSearch
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Data.List.NonEmpty as NE
import Data.Time (diffUTCTime)
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import qualified Storage.Queries.DriverLocation as DrLoc
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as QPI
import Types.API.Location as Location
import Types.Metrics
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as PI
import Utils.Common hiding (id)
import Prelude (atan2)

missingLocationUpdatesKey :: Id PI.ProductInstance -> Text
missingLocationUpdatesKey (Id rideId) = "BPP:missingLocationUpdates:" <> rideId

updateLocation :: Id Person.Person -> UpdateLocationReq -> FlowHandler APISuccess
updateLocation personId waypoints = withFlowHandlerAPI . withLogTag "driverLocationUpdate" $ do
  driver <-
    Person.findPersonById personId
      >>= fromMaybeM PersonNotFound
  unless (driver.role == Person.DRIVER) $ throwError AccessDenied
  mbLoc <- DrLoc.findById driver.id
  now <- getCurrentTime
  refreshPeriod <- asks (.updateLocationRefreshPeriod) <&> fromIntegral
  distanceMb <- case mbLoc of
    Just loc ->
      if now `diffUTCTime` loc.updatedAt > refreshPeriod
        then
          let lastWaypoint =
                Waypoint
                  { pt = locationToLatLong loc,
                    ts = loc.updatedAt,
                    acc = Nothing
                  }
              traversedWaypoints = lastWaypoint : waypointList
           in calcDistanceMb driver.id traversedWaypoints
        else logWarning "Called before refresh period passed, ignoring" $> Nothing
    Nothing -> calcDistanceMb driver.id waypointList
  DB.runSqlDBTransaction $ do
    whenJust distanceMb $ QPI.updateDistance driver.id
    DrLoc.upsertGpsCoord driver.id currPoint.pt currPoint.ts
  logInfo $ getId personId <> " " <> encodeToText waypoints
  return Success
  where
    currPoint = NE.last waypoints
    waypointList = NE.toList waypoints
    checkWaypointsForMissingUpdates allowedDelay wps =
      or $ zipWith (\a b -> b.ts `diffUTCTime` a.ts > allowedDelay) wps (tail wps)
    calcDistanceMb driverId wps =
      QPI.getInProgressByDriverId driverId >>= \case
        Just ride -> do
          missingLocationUpdates <-
            Redis.getKeyRedis @() (missingLocationUpdatesKey ride.id)
              <&> maybe False (const True)
          allowedDelay <- asks (.updateLocationAllowedDelay) <&> fromIntegral
          let missingUpdates =
                missingLocationUpdates
                  || checkWaypointsForMissingUpdates allowedDelay wps
          if missingUpdates
            then Redis.setExRedis (missingLocationUpdatesKey ride.id) () (60 * 60 * 24) $> Nothing
            else pure . Just . getRouteLinearLength $ map (.pt) wps
        Nothing -> logWarning "No ride is assigned to driver, ignoring" $> Nothing

getLocation :: Id PI.ProductInstance -> FlowHandler GetLocationRes
getLocation piId = withFlowHandlerAPI $ do
  ride <-
    QPI.findByParentIdType piId Case.RIDEORDER
      >>= fromMaybeM PIDoesNotExist
  status <-
    case ride.status of
      PI.TRIP_ASSIGNED -> pure PreRide
      PI.INPROGRESS -> pure ActualRide
      _ -> throwError $ PIInvalidStatus "Cannot track this ride"
  driver <-
    (ride.personId & fromMaybeM (PIFieldNotPresent "person_id"))
      >>= Person.findPersonById
      >>= fromMaybeM PersonNotFound
  currLocation <-
    DrLoc.findById driver.id
      >>= fromMaybeM LocationNotFound
  let lastUpdate = currLocation.updatedAt
  let totalDistance = ride.traveledDistance
      currPoint = locationToLatLong currLocation
  return $ GetLocationRes {..}

locationToLatLong :: (HasField "lat" a Double, HasField "long" a Double) => a -> MapSearch.LatLong
locationToLatLong loc =
  MapSearch.LatLong loc.lat loc.long

getRoute' ::
  ( CoreMetrics m,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl]
  ) =>
  [MapSearch.LatLong] ->
  m (Maybe MapSearch.Route)
getRoute' = MapSearch.getRouteMb (Just MapSearch.CAR)

getRoutes :: Id Person.Person -> Location.Request -> FlowHandler Location.Response
getRoutes _ = withFlowHandlerAPI . MapSearch.getRoutes

calculateDistance ::
  ( CoreMetrics m,
    HasFlowEnv m r '["graphhopperUrl" ::: BaseUrl]
  ) =>
  LatLong ->
  LatLong ->
  m (Maybe Double)
calculateDistance sourceLoc destinationLoc = do
  MapSearch.getDistanceMb (Just MapSearch.CAR) [sourceLoc, destinationLoc]

distanceBetweenInMeters :: LatLong -> LatLong -> Double
distanceBetweenInMeters (LatLong lat1 lon1) (LatLong lat2 lon2) =
  -- Calculating using haversine formula
  let r = 6371000 -- Radius of earth in meters
      dlat = deg2Rad $ lat2 - lat1
      dlon = deg2Rad $ lon2 - lon1
      rlat1 = deg2Rad lat1
      rlat2 = deg2Rad lat2
      sq x = x * x
      -- Calculated distance is real (not imaginary) when 0 <= h <= 1
      -- Ideally in our use case h wouldn't go out of bounds
      h = sq (sin (dlat / 2)) + cos rlat1 * cos rlat2 * sq (sin (dlon / 2))
   in 2 * r * atan2 (sqrt h) (sqrt (1 - h))

deg2Rad :: Double -> Double
deg2Rad degree = degree * pi / 180

getRouteLinearLength :: [LatLong] -> Double
getRouteLinearLength pts@(_ : t) = sum $ zipWith distanceBetweenInMeters pts t
getRouteLinearLength _ = 0
