{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverLocation where

import Domain.Types.DriverLocation
import Domain.Types.Person as Person
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.DriverInformation as CDI
import qualified Storage.Queries.DriverLocation as DLQueries

upsertGpsCoord :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person -> LatLong -> UTCTime -> m ()
upsertGpsCoord driverId latLong calculationTime = do
  driverInfo <- CDI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  if not driverInfo.onRide -- if driver not on ride directly save location updates to DB
    then void $ Esq.runNoTransaction $ DLQueries.upsertGpsCoord driverId latLong calculationTime
    else do
      mOldLocation <- findById driverId
      case mOldLocation of
        Nothing -> do
          driverLocation <- Esq.runNoTransaction $ DLQueries.upsertGpsCoord driverId latLong calculationTime
          cacheDriverLocation driverLocation
        Just oldLoc -> do
          now <- getCurrentTime
          cacheDriverLocation $ oldLoc{updatedAt = now, lat = latLong.lat, lon = latLong.lon, coordinatesCalculatedAt = calculationTime}

makeDriverLocationKey :: Id Person -> Text
makeDriverLocationKey id = "DriverLocation:PersonId-" <> id.getId

findById :: (CacheFlow m r, EsqDBReplicaFlow m r) => Id Person -> m (Maybe DriverLocation)
findById id =
  Hedis.get (makeDriverLocationKey id) >>= \case
    Just a ->
      return $ Just a
    Nothing ->
      flip whenJust cacheDriverLocation /=<< Esq.runInReplica (DLQueries.findById id)

cacheDriverLocation :: (CacheFlow m r) => DriverLocation -> m ()
cacheDriverLocation driverLocation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let driverLocationKey = makeDriverLocationKey driverLocation.driverId
  Hedis.setExp driverLocationKey driverLocation expTime

updateOnRide :: (CacheFlow m r, Esq.EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person.Driver -> Bool -> m ()
updateOnRide driverId onRide = do
  if onRide
    then do
      -- driver coming to ride, update location from db to redis
      driverLocation <- DLQueries.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
      cacheDriverLocation driverLocation
    else do
      -- driver going out of ride, update location from redis to db
      mDriverLocatation <- findById (cast driverId)
      maybe
        (pure ())
        ( \loc -> do
            let latLong = LatLong loc.lat loc.lon
            void $ Esq.runTransaction $ DLQueries.upsertGpsCoord (cast driverId) latLong loc.coordinatesCalculatedAt
        )
        mDriverLocatation
  CDI.updateOnRide driverId onRide
