module Types.API.RideBooking where

import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Domain.Types.BookingLocation (BookingLocationAPIEntity)
import Domain.Types.FareBreakup (FareBreakupAPIEntity)
import Domain.Types.Ride (RideAPIEntity)
import Domain.Types.RideBooking (RideBooking, RideBookingStatus)
import EulerHS.Prelude hiding (id)

data RideBookingStatusRes = RideBookingStatusRes
  { id :: Id RideBooking,
    status :: RideBookingStatus,
    agencyName :: Text,
    agencyNumber :: Text,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    toLocation :: Maybe BookingLocationAPIEntity,
    fromLocation :: BookingLocationAPIEntity,
    baseDistance :: Maybe Kilometers,
    baseDuration :: Maybe Hours,
    rideList :: [RideAPIEntity],
    tripTerms :: [Text],
    fareBreakup :: [FareBreakupAPIEntity],
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype RideBookingListRes = RideBookingListRes
  { list :: [RideBookingStatusRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
