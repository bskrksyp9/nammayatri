{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RideBooking where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Types.BookingLocation as DLoc
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RentalSlab as DRentalSlab
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)
import Servant.API

data RideBookingStatus
  = NEW
  | CONFIRMED
  | AWAITING_REASSIGNMENT
  | COMPLETED
  | CANCELLED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData RideBookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData RideBookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data BPPRideBooking

data RideBooking = RideBooking
  { id :: Id RideBooking,
    bppBookingId :: Maybe (Id BPPRideBooking),
    status :: RideBookingStatus,
    providerId :: Text,
    providerUrl :: BaseUrl,
    providerName :: Text,
    providerMobileNumber :: Text,
    startTime :: UTCTime,
    riderId :: Id DPerson.Person,
    fromLocationId :: Id DLoc.BookingLocation,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    vehicleVariant :: VehicleVariant,
    rideBookingDetails :: RideBookingDetails,
    tripTerms :: Maybe DTripTerms.TripTerms,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

data RideBookingDetails = OneWayDetails OneWayRideBookingDetails | RentalDetails RentalRideBookingDetails
  deriving (Show)

data OneWayRideBookingDetails = OneWayRideBookingDetails
  { toLocationId :: Id DLoc.BookingLocation,
    distance :: HighPrecMeters
  }
  deriving (Show)

-- the same as RentalSlab and RentalQuoteDetails
data RentalRideBookingDetails = RentalRideBookingDetails
  { slabId :: Id DRentalSlab.RentalSlab,
    baseDistance :: Kilometers,
    baseDuration :: Hours
  }
  deriving (Show)

mkRentalQuoteDetails :: DRentalSlab.RentalSlab -> RentalRideBookingDetails
mkRentalQuoteDetails DRentalSlab.RentalSlab {..} =
  RentalRideBookingDetails
    { slabId = id,
      ..
    }
