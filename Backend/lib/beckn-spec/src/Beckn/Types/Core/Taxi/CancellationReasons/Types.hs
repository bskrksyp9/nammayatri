{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.CancellationReasons.Types where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype CancellationReasonCode = CancellationReasonCode Text
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

data CancellationReason = CancellationReason
  { reasonCode :: CancellationReasonCode,
    description :: Text,
    enabled :: Bool,
    priority :: Int
  }
  deriving (Generic, ToJSON, ToSchema, Show, FromJSON)

type CancellationReasons = [CancellationReason]

data CancellationReasonsReq = CancellationReasonsReq
  { order_id :: Text,
    item_id :: Text
  }
  deriving (Generic, ToJSON, ToSchema, Show, FromJSON)
