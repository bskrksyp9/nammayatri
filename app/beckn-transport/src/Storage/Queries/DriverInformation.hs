module Storage.Queries.DriverInformation where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.ID
import Beckn.Utils.Common (HasSchemaName (..), getCurrTime)
import Beckn.Types.Storage.Person (Driver)
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.DriverInformation as DriverInformation

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity DriverInformation.DriverInformationT))
getDbTable = DB._driverInformation . DB.transporterDb <$> getSchemaName

create :: DriverInformation.DriverInformation -> Flow ()
create DriverInformation.DriverInformation {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression DriverInformation.DriverInformation {..})
    >>= either DB.throwDBError pure

findById :: ID Driver -> Flow (Maybe DriverInformation.DriverInformation)
findById driverId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate DriverInformation.DriverInformation {..} = _driverId ==. B.val_ driverId

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

fetchAllAvailableByIds :: [ID Driver] -> Flow [DriverInformation.DriverInformation]
fetchAllAvailableByIds driversIds = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable predicate
  where
    predicate DriverInformation.DriverInformation {..} =
      foldr
        (&&.)
        (B.val_ True)
        [ _driverId `B.in_` (B.val_ <$> driversIds),
          _active ==. B.val_ True,
          _onRide ==. B.val_ False
        ]

updateActivity :: ID Driver -> Bool -> Flow ()
updateActivity driverId active = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause active now) (predicate driverId)
    >>= either DB.throwDBError pure
  where
    setClause a now DriverInformation.DriverInformation {..} =
      mconcat
        [ _active <-. B.val_ a,
          _updatedAt <-. B.val_ now
        ]
    predicate id DriverInformation.DriverInformation {..} = _driverId ==. B.val_ id

updateOnRideFlow :: ID Driver -> Bool -> Flow ()
updateOnRideFlow driverId onRide =
  DB.runSqlDB (updateOnRide driverId onRide)
    >>= either DB.throwDBError pure

updateOnRide ::
  ID Driver ->
  Bool ->
  DB.SqlDB ()
updateOnRide driverId onRide = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update' dbTable (setClause onRide now) (predicate driverId)
  where
    setClause onR now' DriverInformation.DriverInformation {..} =
      mconcat
        [ _onRide <-. B.val_ onR,
          _updatedAt <-. B.val_ now'
        ]
    predicate id DriverInformation.DriverInformation {..} = _driverId ==. B.val_ id

deleteById :: ID Driver -> Flow ()
deleteById driverId = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate driverId)
    >>= either DB.throwDBError pure
  where
    predicate pid DriverInformation.DriverInformation {..} = _driverId ==. B.val_ pid
