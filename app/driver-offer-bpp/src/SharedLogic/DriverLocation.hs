module SharedLogic.DriverLocation where

import Beckn.External.Maps
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.DriverInformation
import Domain.Types.DriverLocation
import Domain.Types.Person as Person
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.DriverInformation as Queries
import qualified Storage.Queries.DriverLocation as DLQueries

upsertGpsCoord :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Person -> LatLong -> UTCTime -> m ()
upsertGpsCoord driverId latLong calculationTime = do
  driverInfo <- findDriverInfoById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  oldLocation <- findById driverId
  if not driverInfo.onRide || isNothing oldLocation
    then do
      driverLocation <- Esq.runTransaction $ DLQueries.upsertGpsCoord driverId latLong calculationTime
      cacheDriverLocation driverLocation
    else do
      let Just oldLoc = oldLocation
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

makeDriverInformationIdKey :: Id Person.Driver -> Text
makeDriverInformationIdKey id = "CachedQueries:DriverInformation:DriverId-" <> id.getId

cacheDriverInformation :: (CacheFlow m r) => Id Person.Driver -> DriverInformation -> m ()
cacheDriverInformation driverId driverInfo = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeDriverInformationIdKey driverId) driverInfo expTime

findDriverInfoById :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person.Driver -> m (Maybe DriverInformation)
findDriverInfoById id =
  Hedis.get (makeDriverInformationIdKey id) >>= \case
    Just a -> pure $ Just a
    Nothing -> flip whenJust (cacheDriverInformation id) /=<< Queries.findById id

clearDriverInfoCache :: (CacheFlow m r) => Id Person.Driver -> m ()
clearDriverInfoCache = Hedis.del . makeDriverInformationIdKey