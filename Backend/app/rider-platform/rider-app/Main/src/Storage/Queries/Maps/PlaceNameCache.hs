{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Maps.PlaceNameCache
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Maps.PlaceNameCache
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.Maps.PlaceNameCache

create :: PlaceNameCache -> SqlDB ()
create = Esq.create

findPlaceByPlaceId :: Transactionable m => Text -> m [PlaceNameCache]
findPlaceByPlaceId placeId =
  Esq.findAll $ do
    placeNameCache <- from $ table @PlaceNameCacheT
    where_ $ placeNameCache ^. PlaceNameCachePlaceId ==. val (Just placeId)
    return placeNameCache

findPlaceByLatLong :: Transactionable m => Double -> Double -> m (Maybe PlaceNameCache)
findPlaceByLatLong lat lon =
  Esq.findOne $ do
    placeNameCache <- from $ table @PlaceNameCacheT
    where_ $
      placeNameCache ^. PlaceNameCacheLat ==. val lat
        &&. placeNameCache ^. PlaceNameCacheLat ==. val lon

    return placeNameCache

findPlaceByGeoHash :: Transactionable m => Text -> m [PlaceNameCache]
findPlaceByGeoHash geoHash =
  Esq.findAll $ do
    placeNameCache <- from $ table @PlaceNameCacheT
    where_ $ placeNameCache ^. PlaceNameCacheGeoHash ==. val (Just geoHash)

    return placeNameCache
