{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.ProviderPlatform.Ride
  ( module Dashboard.ProviderPlatform.Ride,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.CancellationReasons.Types as Reexport (CancellationReasonCode (..))
import Dashboard.Common as Reexport
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common
import Kernel.Types.Id
import Servant hiding (Summary)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data RideEndpoint
  = RideStartEndpoint
  | RideEndEndpoint
  | RideCancelEndpoint
  | RideSyncEndpoint
  deriving (Show, Read)

derivePersistField "RideEndpoint"

---------------------------------------------------------
-- ride list --------------------------------------------

type RideListAPI =
  "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "bookingStatus" BookingStatus
    :> QueryParam "rideShortId" (ShortId Ride)
    :> QueryParam "customerPhoneNo" Text
    :> QueryParam "driverPhoneNo" Text
    :> QueryParam "fareDiff" Money
    :> Get '[JSON] RideListRes

data RideListRes = RideListRes
  { totalItems :: Int, -- for backward compatibility
    summary :: Summary,
    rides :: [RideListItem]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListItem = RideListItem
  { rideId :: Id Ride,
    rideShortId :: ShortId Ride,
    customerName :: Maybe Text,
    customerPhoneNo :: Text,
    driverName :: Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Text,
    fareDiff :: Maybe Money,
    bookingStatus :: BookingStatus,
    rideCreatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BookingStatus = UPCOMING | UPCOMING_6HRS | ONGOING | ONGOING_6HRS | COMPLETED | CANCELLED
  deriving stock (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

derivePersistField "BookingStatus"

-- TODO move similar instances to Lib
instance FromHttpApiData BookingStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData BookingStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

---------------------------------------------------------
-- ride start -------------------------------------------

type RideStartAPI =
  Capture "rideId" (Id Ride)
    :> "start"
    :> ReqBody '[JSON] StartRideReq
    :> Post '[JSON] APISuccess

newtype StartRideReq = StartRideReq
  { point :: Maybe LatLong
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets StartRideReq where
  hideSecrets = identity

---------------------------------------------------------
-- ride end ---------------------------------------------

type RideEndAPI =
  Capture "rideId" (Id Ride)
    :> "end"
    :> ReqBody '[JSON] EndRideReq
    :> Post '[JSON] APISuccess

newtype EndRideReq = EndRideReq
  { point :: Maybe LatLong
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets EndRideReq where
  hideSecrets = identity

---------------------------------------------------------
-- ride cancel ------------------------------------------

type RideCancelAPI =
  Capture "rideId" (Id Ride)
    :> "cancel"
    :> ReqBody '[JSON] CancelRideReq
    :> Post '[JSON] APISuccess

data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets CancelRideReq where
  hideSecrets = identity

---------------------------------------------------------
-- ride info --------------------------------------------

type RideInfoAPI =
  Capture "rideId" (Id Ride)
    :> "info"
    :> Get '[JSON] RideInfoRes

data RideInfoRes = RideInfoRes
  { rideId :: Id Ride,
    customerName :: Maybe Text,
    customerPhoneNo :: Text,
    rideOtp :: Text,
    customerPickupLocation :: LocationAPIEntity,
    customerDropLocation :: Maybe LocationAPIEntity,
    actualDropLocation :: Maybe LatLong,
    driverId :: Id Driver,
    driverName :: Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Text,
    driverStartLocation :: Maybe LatLong,
    driverCurrentLocation :: Maybe LatLong,
    rideBookingTime :: UTCTime,
    estimatedDriverArrivalTime :: Maybe UTCTime,
    actualDriverArrivalTime :: Maybe UTCTime,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    rideDistanceEstimated :: Maybe Meters,
    rideDistanceActual :: Meters,
    chargeableDistance :: Maybe Meters,
    maxEstimatedDistance :: Maybe Meters,
    estimatedRideDuration :: Maybe Minutes,
    estimatedFare :: Money,
    actualFare :: Maybe Money,
    driverOfferedFare :: Maybe Money,
    pickupDuration :: Maybe Minutes,
    rideDuration :: Maybe Minutes,
    bookingStatus :: BookingStatus,
    cancelledTime :: Maybe UTCTime,
    cancelledBy :: Maybe CancellationSource,
    cancellationReason :: Maybe CancellationReasonCode,
    driverInitiatedCallCount :: Int,
    bookingToRideStartDuration :: Maybe Minutes,
    distanceCalculationFailed :: Maybe Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationAPIEntity = LocationAPIEntity
  { lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  | ByApplication
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- ride sync ---------------------------------------------

type RideSyncAPI =
  Capture "rideId" (Id Ride)
    :> "sync"
    :> Post '[JSON] RideSyncRes

data RideSyncRes = RideSyncRes
  { newStatus :: RideStatus,
    message :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets RideSyncRes where
  hideSecrets = identity

data RideStatus
  = RIDE_NEW
  | RIDE_INPROGRESS
  | RIDE_COMPLETED
  | RIDE_CANCELLED
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- ride route -------------------------------------------

type RideRouteAPI =
  Capture "rideId" (Id Ride)
    :> "route"
    :> Post '[JSON] RideRouteRes

data ActualRoute = ActualRoute
  { lat :: Double,
    lon :: Double,
    timestamp :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype RideRouteRes = RideRouteRes
  { actualRoute :: [ActualRoute]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverEdaKafka = DriverEdaKafka
  { driver_id :: String,
    rid :: Maybe String,
    ts :: String,
    lat :: Maybe String,
    lon :: Maybe String,
    mid :: Maybe String,
    updated_at :: Maybe String,
    created_at :: Maybe String,
    on_ride :: Maybe String,
    active :: Maybe String,
    partition_date :: String,
    date :: String
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

instance HideSecrets RideRouteRes where
  hideSecrets = identity
