module Storage.Queries.Tracker where

import Beckn.Types.Common
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Tracker as Storage

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.TrackerT)
dbTable = DB._tracker DB.transporterDb

create :: Storage.Tracker -> L.Flow ()
create Storage.Tracker {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Tracker {..})
    >>= either DB.throwDBError pure

findTrackerById ::
  TrackerId -> L.Flow (Maybe Storage.Tracker)
findTrackerById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Tracker {..} = _id ==. B.val_ id
