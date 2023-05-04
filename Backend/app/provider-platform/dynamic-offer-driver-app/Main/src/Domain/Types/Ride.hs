{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Ride where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.Booking as DRB
import Domain.Types.Booking.TripLocation as DTripLocation
import qualified Domain.Types.FareParameters as DFare
import qualified Domain.Types.Person as DPers
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types
import qualified Kernel.Prelude as BP
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant.API

data RideStatus
  = NEW
  | INPROGRESS
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, BP.ToParamSchema)

instance FromHttpApiData RideStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data Ride = Ride
  { id :: Id Ride,
    bookingId :: Id DRB.Booking,
    shortId :: ShortId Ride,
    status :: RideStatus,
    driverId :: Id DPers.Person,
    otp :: Text,
    trackingUrl :: BaseUrl,
    fare :: Maybe Money,
    traveledDistance :: HighPrecMeters,
    chargeableDistance :: Maybe Meters,
    driverArrivalTime :: Maybe UTCTime,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime,
    tripStartPos :: Maybe LatLong,
    tripEndPos :: Maybe LatLong,
    fareParametersId :: Maybe (Id DFare.FareParameters),
    distanceCalculationFailed :: Maybe Bool,
    fromLocationId :: Id DTripLocation.TripLocation,
    toLocationId :: Id DTripLocation.TripLocation,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
