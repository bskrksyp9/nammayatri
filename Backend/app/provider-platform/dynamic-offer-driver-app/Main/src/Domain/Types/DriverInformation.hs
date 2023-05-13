{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.DriverInformation where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToParamSchema, ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time (UTCTime)
import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.Person (Person)
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import Servant.API

data DriverMode
  = ONLINE
  | OFFLINE
  | SILENT
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
  deriving (PrettyShow) via Showable DriverMode

derivePersistField "DriverMode"

instance FromHttpApiData DriverMode where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData DriverMode where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data DriverInformationE e = DriverInformation
  { driverId :: Id Person,
    adminId :: Maybe (Id Person),
    merchantId :: Id DMerchant.Merchant,
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    verified :: Bool,
    referralCode :: Maybe (EncryptedHashedField e Text),
    lastEnabledOn :: Maybe UTCTime,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    canDowngradeToTaxi :: Bool,
    mode :: Maybe DriverMode,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type DriverInformation = DriverInformationE 'AsEncrypted

instance FromJSON (EncryptedHashed Text)

instance ToJSON (EncryptedHashed Text)

instance FromJSON DriverInformation

instance ToJSON DriverInformation
