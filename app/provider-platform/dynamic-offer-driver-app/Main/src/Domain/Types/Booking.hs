module Domain.Types.Booking where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import qualified Domain.Types.Booking.BookingLocation as DLoc
import Domain.Types.DriverQuote
import Domain.Types.FareParameters (FareParameters)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.RiderDetails as DRD
import qualified Domain.Types.Vehicle.Variant as DVeh
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Servant.API

data BookingStatus
  = NEW
  | TRIP_ASSIGNED
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromHttpApiData BookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData BookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

data Booking = Booking
  { id :: Id Booking,
    quoteId :: Id DriverQuote,
    status :: BookingStatus,
    providerId :: Id DM.Merchant,
    bapId :: Text,
    bapUri :: BaseUrl,
    startTime :: UTCTime,
    riderId :: Maybe (Id DRD.RiderDetails),
    fromLocation :: DLoc.BookingLocation,
    toLocation :: DLoc.BookingLocation,
    vehicleVariant :: DVeh.Variant,
    estimatedDistance :: Meters,
    estimatedFare :: Money,
    estimatedDuration :: Seconds,
    fareParams :: FareParameters,
    riderName :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)