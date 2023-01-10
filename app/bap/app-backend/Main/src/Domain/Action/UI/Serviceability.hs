module Domain.Action.UI.Serviceability
  ( checkServiceability,
  )
where

import Beckn.External.Maps.Types
import Beckn.Prelude
import Beckn.Storage.Hedis
import Beckn.Types.Geofencing
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Person as Person
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerchant
import Storage.Queries.Geometry (someGeometriesContain)
import qualified Storage.Queries.Person as QP
import Tools.Error

checkServiceability ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  (GeofencingConfig -> GeoRestriction) ->
  Id Person.Person ->
  LatLong ->
  m Bool
checkServiceability settingAccessor personId location = do
  person <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  let merchId = person.merchantId
  geoConfig <- fmap (.geofencingConfig) $ QMerchant.findById merchId >>= fromMaybeM (MerchantNotFound merchId.getId)
  let geoRestriction = settingAccessor geoConfig
  case geoRestriction of
    Unrestricted -> pure True
    Regions regions -> someGeometriesContain location regions