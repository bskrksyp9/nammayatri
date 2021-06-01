{-# LANGUAGE TypeApplications #-}

module Beckn.Product.MapSearch where

import qualified Beckn.External.Graphhopper.Flow as Grphr
import qualified Beckn.External.Graphhopper.Types as Grphr
import Beckn.Types.Common
import Beckn.Types.Error.API
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Types.Monitoring.Prometheus.Metrics (HasCoreMetrics)
import Beckn.Utils.Common
import Data.Geospatial
import EulerHS.Prelude
import GHC.Records (HasField (..))
import Prelude (atan2)

getRouteMb ::
  ( HasField "graphhopperUrl" r BaseUrl,
    HasCoreMetrics r
  ) =>
  MapSearch.Request ->
  FlowR r (Maybe MapSearch.Route)
getRouteMb request =
  (listToMaybe . (^. #routes) <$> getRoute request)
    `catch` \(_ :: RouteError) -> pure Nothing

getRoute ::
  ( HasField "graphhopperUrl" r BaseUrl,
    HasCoreMetrics r
  ) =>
  MapSearch.Request ->
  FlowR r MapSearch.Response
getRoute MapSearch.Request {..} = do
  -- Currently integrated only with graphhopper
  unless (all isLatLong waypoints) $ throwError RouteNotLatLong
  let points = map (\(MapSearch.LatLong point) -> point) waypoints
      mode' = fromMaybe MapSearch.CAR mode
      vehicle = mapToVehicle mode'
  graphhopperUrl <- getField @"graphhopperUrl" <$> ask
  Grphr.Response {..} <-
    Grphr.search graphhopperUrl (grphrReq points vehicle)
      >>= fromEitherM (RouteRequestError graphhopperUrl)
  return $
    MapSearch.Response
      { status = "OK",
        routes = mapToRoute mode' <$> _paths
      }
  where
    isLatLong :: MapSearch.MapPoint -> Bool
    isLatLong (MapSearch.LatLong _) = True
    isLatLong _ = False
    grphrReq :: [PointXY] -> Grphr.Vehicle -> Grphr.Request
    grphrReq points vehicle =
      Grphr.Request
        { _points' = points,
          _vehicle = vehicle,
          _weighting = Nothing,
          _elevation = Nothing,
          _calcPoints = calcPoints
        }

mapToVehicle :: MapSearch.TravelMode -> Grphr.Vehicle
mapToVehicle MapSearch.CAR = Grphr.CAR
mapToVehicle MapSearch.MOTORCYCLE = Grphr.SCOOTER
mapToVehicle MapSearch.BICYCLE = Grphr.BIKE
mapToVehicle MapSearch.FOOT = Grphr.FOOT

mapToRoute :: MapSearch.TravelMode -> Grphr.Path -> MapSearch.Route
mapToRoute mode Grphr.Path {..} =
  MapSearch.Route
    { distanceInM = _distance,
      durationInS = div _time 1000,
      boundingBox = _bbox,
      snapped_waypoints = Just _snapped_waypoints,
      mode = mode,
      points = _points
    }

deg2Rad :: Double -> Double
deg2Rad degree = degree * pi / 180

distanceBetweenInMeters :: PointXY -> PointXY -> Float
distanceBetweenInMeters (PointXY lat1 lon1) (PointXY lat2 lon2) =
  -- Calculating using haversine formula
  let r = 6371000 -- Radius of earth in meters
      dlat = deg2Rad $ lat2 - lat1
      dlon = deg2Rad $ lon2 - lon1
      rlat1 = deg2Rad lat1
      rlat2 = deg2Rad lat2
      -- Calculated distance is real (not imaginary) when 0 <= h <= 1
      -- Ideally in our use case h wouldn't go out of bounds
      h = (sin (dlat / 2) ^ (2 :: Integer)) + cos rlat1 * cos rlat2 * (sin (dlon / 2) ^ (2 :: Integer))
   in -- Float precision for distance is sufficient as we are working with `meter` units
      realToFrac $ 2 * r * atan2 (sqrt h) (sqrt (1 - h))

speedInMPS :: Float -> Integer -> Float
speedInMPS distance duration =
  if duration <= 0
    then 0 -- Realistically this is not possible, so just returning zero
    else distance / fromIntegral duration
