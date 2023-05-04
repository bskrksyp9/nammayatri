{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Booking.Type where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.Booking.TripLocation as DLoc
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RentalSlab as DRentalSlab
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Servant.API

activeBookingStatus :: [BookingStatus]
activeBookingStatus = [NEW, CONFIRMED, AWAITING_REASSIGNMENT, TRIP_ASSIGNED]

data BookingStatus
  = NEW
  | CONFIRMED
  | AWAITING_REASSIGNMENT
  | REALLOCATED
  | COMPLETED
  | CANCELLED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance FromHttpApiData BookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData BookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data BPPBooking

data Booking = Booking
  { id :: Id Booking,
    transactionId :: Text,
    bppBookingId :: Maybe (Id BPPBooking),
    status :: BookingStatus,
    providerId :: Text,
    quoteId :: Maybe (Id DQuote.Quote),
    providerUrl :: BaseUrl,
    providerName :: Text,
    providerMobileNumber :: Text,
    primaryExophone :: Text,
    startTime :: UTCTime,
    riderId :: Id DPerson.Person,
    fromLocation :: DLoc.TripLocation,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    vehicleVariant :: VehicleVariant,
    bookingDetails :: BookingDetails,
    tripTerms :: Maybe DTripTerms.TripTerms,
    merchantId :: Id DMerchant.Merchant,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

data BookingDetails
  = OneWayDetails OneWayBookingDetails
  | RentalDetails DRentalSlab.RentalSlab
  | DriverOfferDetails OneWayBookingDetails
  | OneWaySpecialZoneDetails OneWaySpecialZoneBookingDetails
  deriving (Show)

data OneWayBookingDetails = OneWayBookingDetails
  { toLocation :: DLoc.TripLocation,
    distance :: HighPrecMeters
  }
  deriving (Show)

data OneWaySpecialZoneBookingDetails = OneWaySpecialZoneBookingDetails
  { toLocation :: DLoc.TripLocation,
    distance :: HighPrecMeters,
    otpCode :: Maybe Text
  }
  deriving (Show)
