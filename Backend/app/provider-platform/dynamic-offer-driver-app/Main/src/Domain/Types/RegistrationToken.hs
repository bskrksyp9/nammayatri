{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RegistrationToken where

import Data.Aeson
import Data.Time
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id

data Medium
  = SMS
  | EMAIL
  deriving (Generic, FromJSON, ToJSON, Eq, Show, Read)

data RTEntityType
  = CUSTOMER
  | USER
  deriving (Generic, FromJSON, ToJSON, Eq, Show, Read)

data LoginType
  = OTP
  | PASSWORD
  deriving (Generic, FromJSON, ToJSON, Eq, Show, Read)

data RegistrationToken = RegistrationToken
  { id :: Id RegistrationToken,
    token :: Text,
    attempts :: Int,
    authMedium :: Medium,
    authType :: LoginType,
    authValueHash :: Text,
    verified :: Bool,
    authExpiry :: Int,
    tokenExpiry :: Int,
    entityId :: Text,
    merchantId :: Text,
    entityType :: RTEntityType,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    info :: Maybe Text,
    alternateNumberAttempts :: Int
  }
  deriving (Generic, Show, Eq)
