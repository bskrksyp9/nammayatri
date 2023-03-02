{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Feedback
  ( FeedbackReq (..),
    FeedbackRes (..),
    feedback,
  )
where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as DRide
import qualified Environment as App
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data FeedbackReq = FeedbackReq
  { rideId :: Id DRide.Ride,
    rating :: Int,
    feedbackDetails :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data FeedbackRes = FeedbackRes
  { bppBookingId :: Id DBooking.BPPBooking,
    ratingValue :: Int,
    feedbackDetails :: Maybe Text,
    providerId :: Text,
    providerUrl :: BaseUrl
  }

feedback :: FeedbackReq -> App.Flow FeedbackRes
feedback request = do
  let ratingValue = request.rating
  unless (ratingValue `elem` [1 .. 5]) $ throwError InvalidRatingValue
  let rideId = request.rideId
      feedbackDetails = request.feedbackDetails
  ride <- QRide.findById rideId (Proxy @App.Flow) >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.status == DRide.COMPLETED) $ throwError (RideInvalidStatus "Feedback available only for completed rides.")
  booking <- QRB.findById ride.bookingId (Proxy @App.Flow) >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  DB.runTransaction $ do
    QRide.updateRideRating @App.Flow rideId ratingValue
    QPFS.updateStatus booking.riderId DPFS.IDLE
  pure
    FeedbackRes
      { providerId = booking.providerId,
        providerUrl = booking.providerUrl,
        ..
      }
