{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.Booking.TripLocation where

import Domain.Types.Booking.TripLocation
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.Booking.TripLocation

create :: TripLocation -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id TripLocation -> m (Maybe TripLocation)
findById = Esq.findById

updateAddress :: Id TripLocation -> LocationAddress -> SqlDB ()
updateAddress blId LocationAddress {..} = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ TripLocationStreet =. val street,
        TripLocationCity =. val city,
        TripLocationState =. val state,
        TripLocationCountry =. val country,
        TripLocationBuilding =. val building,
        TripLocationAreaCode =. val areaCode,
        TripLocationArea =. val area,
        TripLocationUpdatedAt =. val now
      ]
    where_ $ tbl ^. TripLocationTId ==. val (toKey blId)
