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

module Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection where

import qualified Domain.Types.FarePolicy as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMoney, Kilometers)
import Kernel.Types.Id
import Storage.Tabular.FarePolicy.Table (FarePolicyTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FarePolicyProgressiveDetailsPerExtraKmRateSectionT sql=fare_policy_progressive_details_per_extra_km_rate_section
      Id Int
      farePolicyId FarePolicyTId
      startDistance Kilometers
      perExtraKmRate HighPrecMoney

      deriving Generic
    |]

type FullFarePolicyProgressiveDetailsPerExtraKmRateSection = (Id Domain.FarePolicy, Domain.FPProgressiveDetailsPerExtraKmRateSection)

instance FromTType FarePolicyProgressiveDetailsPerExtraKmRateSectionT FullFarePolicyProgressiveDetailsPerExtraKmRateSection where
  fromTType FarePolicyProgressiveDetailsPerExtraKmRateSectionT {..} = do
    return
      ( fromKey farePolicyId,
        Domain.FPProgressiveDetailsPerExtraKmRateSection
          { ..
          }
      )

instance ToTType FarePolicyProgressiveDetailsPerExtraKmRateSectionT FullFarePolicyProgressiveDetailsPerExtraKmRateSection where
  toTType (farePolicyId, Domain.FPProgressiveDetailsPerExtraKmRateSection {..}) = do
    FarePolicyProgressiveDetailsPerExtraKmRateSectionT
      { farePolicyId = toKey farePolicyId,
        ..
      }
