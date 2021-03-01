{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.DriverStats where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage.Common
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.ID
import Beckn.Types.Storage.Person (Driver)
import Beckn.Utils.Common (fromMaybeM400, getCurrTime, getSchemaName)
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.DriverStats as Storage

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.DriverStatsT))
getDbTable =
  DB._driverStats . DB.transporterDb <$> getSchemaName

createInitialDriverStats :: ID Driver -> Flow ()
createInitialDriverStats driverId = do
  dbTable <- getDbTable
  now <- getCurrTime
  let driverStats =
        Storage.DriverStats
          { _driverId = driverId,
            _idleSince = now
          }
  DB.createOne dbTable (Storage.Common.insertExpression driverStats)
    >>= either DB.throwDBError pure

getFirstDriverInTheQueue :: [ID Driver] -> Flow (ID Driver)
getFirstDriverInTheQueue ids = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable predicate 1 0 order
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "NO_DRIVERS_NEARBY" . listToMaybe . map (^. #_driverId)
  where
    predicate Storage.DriverStats {..} = _driverId `B.in_` (B.val_ <$> ids)
    order Storage.DriverStats {..} = B.asc_ _idleSince

updateIdleTime :: ID Driver -> Flow ()
updateIdleTime driverId = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause now) (predicate driverId)
    >>= either DB.throwDBError pure
  where
    setClause now Storage.DriverStats {..} =
      mconcat
        [ _idleSince <-. B.val_ now
        ]
    predicate id Storage.DriverStats {..} = _driverId ==. B.val_ id

fetchAll :: Flow [Storage.DriverStats]
fetchAll = do
  dbTable <- getDbTable
  DB.findAllRows dbTable
    >>= either DB.throwDBError pure

deleteById :: ID Driver -> Flow ()
deleteById driverId = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate driverId)
    >>= either DB.throwDBError pure
  where
    predicate dId Storage.DriverStats {..} = _driverId ==. B.val_ dId
