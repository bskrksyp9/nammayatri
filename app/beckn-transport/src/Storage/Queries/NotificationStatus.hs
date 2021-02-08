module Storage.Queries.NotificationStatus where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Utils.Common (getSchemaName)
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App (DriverId, RideId)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.NotificationStatus as NotificationStatus

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity NotificationStatus.NotificationStatusT))
getDbTable =
  DB._notificationStatus . DB.transporterDb <$> getSchemaName

create :: NotificationStatus.NotificationStatus -> Flow ()
create NotificationStatus.NotificationStatus {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression NotificationStatus.NotificationStatus {..})
    >>= either DB.throwDBError pure

updateStatus :: RideId -> DriverId -> NotificationStatus.AnswerStatus -> Flow ()
updateStatus rideId driverId status = do
  dbTable <- getDbTable
  DB.update dbTable (setClause status) (predicate rideId driverId)
    >>= either DB.throwDBError pure
  where
    setClause s NotificationStatus.NotificationStatus {..} = _status <-. B.val_ s
    predicate rId dId NotificationStatus.NotificationStatus {..} =
      _rideId ==. B.val_ rId
        &&. _driverId ==. B.val_ dId

fetchByRideIdAndDriverId :: RideId -> DriverId -> Flow (Maybe NotificationStatus.NotificationStatus)
fetchByRideIdAndDriverId rideId driverId = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate rideId driverId)
    >>= either DB.throwDBError pure
  where
    predicate rId dId NotificationStatus.NotificationStatus {..} =
      _rideId ==. B.val_ rId
        &&. _driverId ==. B.val_ dId

fetchRefusedNotificationsByRideId :: RideId -> Flow [NotificationStatus.NotificationStatus]
fetchRefusedNotificationsByRideId rideId = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      _rideId ==. B.val_ rideId
        &&. _status `B.in_` [B.val_ NotificationStatus.REJECTED, B.val_ NotificationStatus.IGNORED]

fetchActiveNotifications :: Flow [NotificationStatus.NotificationStatus]
fetchActiveNotifications = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      _status ==. B.val_ NotificationStatus.NOTIFIED

findActiveNotificationByRideId :: RideId -> Flow (Maybe NotificationStatus.NotificationStatus)
findActiveNotificationByRideId rideId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate NotificationStatus.NotificationStatus {..} =
      _rideId ==. B.val_ rideId
        &&. _status ==. B.val_ NotificationStatus.NOTIFIED

findActiveNotificationByDriverId :: DriverId -> Maybe RideId -> Flow (Maybe NotificationStatus.NotificationStatus)
findActiveNotificationByDriverId driverId rideId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate NotificationStatus.NotificationStatus {..} =
      _driverId ==. B.val_ driverId
        &&. maybe (B.val_ True) (\v -> _rideId ==. B.val_ v) rideId
        &&. _status ==. B.val_ NotificationStatus.NOTIFIED
