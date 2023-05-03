{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Merchant where

import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant as CQM
import Tools.Error

findTransporter :: Id DM.Merchant -> Flow DM.Merchant
findTransporter transporterId = do
  transporter <- CQM.findById transporterId >>= fromMaybeM (MerchantDoesNotExist transporterId.getId)
  unless transporter.enabled $ throwError AgencyDisabled
  pure transporter

findMerchantByShortId :: ShortId DM.Merchant -> Flow DM.Merchant
findMerchantByShortId merchantShortId = do
  CQM.findByShortId merchantShortId
    >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

checkMerchantExist :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m ()
checkMerchantExist merchantId = do
  _ <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  return ()
