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

module Storage.Tabular.CancellationReason where

import qualified Beckn.Types.Core.Taxi.CancellationReasons.Types as SCR
import qualified Domain.Types.CancellationReason as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto

derivePersistField "Domain.CancellationStage"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    CancellationReasonT sql=cancellation_reason
      reasonCode Text
      description Text
      enabled Bool
      onSearch Bool
      onConfirm Bool
      onAssign Bool
      priority Int
      Primary reasonCode
      deriving Generic
    |]

instance TEntityKey CancellationReasonT where
  type DomainKey CancellationReasonT = SCR.CancellationReasonCode
  fromKey (CancellationReasonTKey _id) = SCR.CancellationReasonCode _id
  toKey (SCR.CancellationReasonCode id) = CancellationReasonTKey id

instance FromTType CancellationReasonT Domain.CancellationReason where
  fromTType CancellationReasonT {..} = do
    return $
      Domain.CancellationReason
        { reasonCode = SCR.CancellationReasonCode reasonCode,
          ..
        }

instance ToTType CancellationReasonT Domain.CancellationReason where
  toTType Domain.CancellationReason {..} =
    CancellationReasonT
      { reasonCode = let (SCR.CancellationReasonCode rc) = reasonCode in rc,
        ..
      }
