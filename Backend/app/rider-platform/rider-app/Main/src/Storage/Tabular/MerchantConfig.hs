{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.MerchantConfig where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantConfig as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Types.SlidingWindowCounters (PeriodType)
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Storage.Tabular.Merchant (MerchantTId)

derivePersistField "PeriodType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MerchantConfigT sql=merchant_config
      merchantId MerchantTId
      fraudBookingDetectionWindow SWC.SlidingWindowOptions
      fraudBookingCancellationCountThreshold Int
      fraudBookingTotalCountThreshold Int
      Primary merchantId
      deriving Generic
    |]

instance TEntityKey MerchantConfigT where
  type DomainKey MerchantConfigT = Id DM.Merchant
  fromKey (MerchantConfigTKey _id) = fromKey _id
  toKey id = MerchantConfigTKey $ toKey id

instance FromTType MerchantConfigT Domain.MerchantConfig where
  fromTType MerchantConfigT {..} = do
    return $
      Domain.MerchantConfig
        { merchantId = fromKey merchantId,
          ..
        }

instance ToTType MerchantConfigT Domain.MerchantConfig where
  toTType Domain.MerchantConfig {..} =
    MerchantConfigT
      { merchantId = toKey merchantId,
        ..
      }
