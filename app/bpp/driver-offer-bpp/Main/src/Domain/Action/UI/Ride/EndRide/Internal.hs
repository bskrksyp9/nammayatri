module Domain.Action.UI.Ride.EndRide.Internal
  ( endRideTransaction,
    putDiffMetric,
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as SRB
import Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as Ride
import EulerHS.Prelude hiding (id)
import SharedLogic.DriverLocation as DLoc
import qualified SharedLogic.Ride as SRide
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverStats as DriverStats
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Metrics as Metrics

endRideTransaction :: (CacheFlow m r, EsqDBFlow m r, Esq.EsqDBReplicaFlow m r) => Id DP.Driver -> Id SRB.Booking -> Ride.Ride -> m ()
endRideTransaction driverId bookingId ride = do
  Esq.runTransaction $ do
    QRide.updateAll ride.id ride
    QRide.updateStatus ride.id Ride.COMPLETED
    QRB.updateStatus bookingId SRB.COMPLETED
    DriverStats.updateIdleTime driverId
  DLoc.updateOnRide driverId False
  SRide.clearCache $ cast driverId

putDiffMetric :: (Metrics.HasBPPMetrics m r, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Money -> Meters -> m ()
putDiffMetric merchantId money mtrs = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  Metrics.putFareAndDistanceDeviations org.name money mtrs