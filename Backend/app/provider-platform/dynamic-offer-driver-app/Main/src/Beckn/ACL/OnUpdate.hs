{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnUpdate
  ( buildOnUpdateMessage,
    OnUpdateBuildReq (..),
    UpdateType (..),
  )
where

import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingCancelledEvent as BookingCancelledOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.DriverArrivedEvent as DriverArrivedOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.EstimateRepetitionEvent as EstimateRepetitionOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent as RideAssignedOU
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as RideCompletedOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent as RideStartedOU
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FareParameters as Fare
import qualified Domain.Types.Person as SP
import Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as SVeh
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator
import Tools.Error

data UpdateType = SIMPLE | FORCED

data OnUpdateBuildReq
  = RideAssignedBuildReq
      { driver :: SP.Person,
        vehicle :: SVeh.Vehicle,
        ride :: DRide.Ride,
        updateType :: UpdateType
      }
  | RideStartedBuildReq
      { ride :: DRide.Ride,
        updateType :: UpdateType
      }
  | RideCompletedBuildReq
      { ride :: DRide.Ride,
        fareParams :: Fare.FareParameters,
        updateType :: UpdateType
      }
  | BookingCancelledBuildReq
      { booking :: DRB.Booking,
        cancellationSource :: SBCR.CancellationSource,
        updateType :: UpdateType
      }
  | DriverArrivedBuildReq
      { ride :: DRide.Ride,
        arrivalTime :: Maybe UTCTime
      }
  | EstimateRepetitionBuildReq
      { ride :: DRide.Ride,
        booking :: DRB.Booking,
        estimateId :: Id DEst.Estimate,
        cancellationSource :: SBCR.CancellationSource
      }

buildOnUpdateMessage ::
  (EsqDBFlow m r, EncFlow m r) =>
  OnUpdateBuildReq ->
  m OnUpdate.OnUpdateMessage
buildOnUpdateMessage RideAssignedBuildReq {..} = do
  mobileNumber <- SP.getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present.")
  name <- SP.getPersonFullName driver >>= fromMaybeM (PersonFieldNotPresent "firstName")
  let agent =
        RideAssignedOU.Agent
          { name = name,
            phone = mobileNumber,
            rating = realToFrac <$> driver.rating,
            tags = RideAssignedOU.AgentTags {registered_at = driver.createdAt}
          }
      veh =
        RideAssignedOU.Vehicle
          { model = vehicle.model,
            variant = show vehicle.variant,
            color = vehicle.color,
            registration = vehicle.registrationNo
          }
      fulfillment =
        RideAssignedOU.FulfillmentInfo
          { id = ride.id.getId,
            start =
              RideAssignedOU.StartInfo
                { authorization =
                    RideAssignedOU.Authorization
                      { _type = "OTP",
                        token = ride.otp
                      }
                },
            vehicle = veh,
            ..
          }
  return $
    OnUpdate.OnUpdateMessage $
      OnUpdate.RideAssigned
        RideAssignedOU.RideAssignedEvent
          { id = ride.bookingId.getId,
            state = "ACTIVE",
            update_target = "state,fufillment.state.code,fulfillment.start.authorization,fulfillment.agent,fulfillment.vehicle",
            fulfillment,
            force = castUpdateType updateType
          }
buildOnUpdateMessage RideStartedBuildReq {..} = do
  return $
    OnUpdate.OnUpdateMessage $
      OnUpdate.RideStarted
        RideStartedOU.RideStartedEvent
          { id = ride.bookingId.getId,
            update_target = "fufillment.state.code",
            fulfillment = RideStartedOU.FulfillmentInfo ride.id.getId,
            force = castUpdateType updateType
          }
buildOnUpdateMessage req@RideCompletedBuildReq {} = do
  fare <- realToFrac <$> req.ride.fare & fromMaybeM (InternalError "Ride fare is not present.")
  chargeableDistance <-
    realToFrac <$> req.ride.chargeableDistance
      & fromMaybeM (InternalError "Ride chargeable distance is not present.")
  let currency = "INR"
      ride = req.ride
      price =
        RideCompletedOU.QuotePrice
          { currency,
            value = fare,
            computed_value = fare
          }
      breakup = mkBreakupList (OnUpdate.BreakupPrice currency . fromIntegral) OnUpdate.BreakupItem req.fareParams
  return $
    OnUpdate.OnUpdateMessage $
      OnUpdate.RideCompleted
        RideCompletedOU.RideCompletedEvent
          { id = ride.bookingId.getId,
            update_target = "fulfillment.state.code,quote.price,quote.breakup",
            quote =
              RideCompletedOU.RideCompletedQuote
                { price,
                  breakup
                },
            fulfillment =
              RideCompletedOU.FulfillmentInfo
                { id = ride.id.getId,
                  chargeable_distance = chargeableDistance
                },
            force = castUpdateType req.updateType
          }
buildOnUpdateMessage BookingCancelledBuildReq {..} = do
  return $
    OnUpdate.OnUpdateMessage $
      OnUpdate.BookingCancelled
        BookingCancelledOU.BookingCancelledEvent
          { id = booking.id.getId,
            state = "CANCELLED",
            update_target = "state,fufillment.state.code",
            cancellation_reason = castCancellationSource cancellationSource,
            force = castUpdateType updateType
          }
buildOnUpdateMessage DriverArrivedBuildReq {..} = do
  return $
    OnUpdate.OnUpdateMessage $
      OnUpdate.DriverArrived
        DriverArrivedOU.DriverArrivedEvent
          { id = ride.bookingId.getId,
            update_target = "state,fufillment.state.code",
            fulfillment = DriverArrivedOU.FulfillmentInfo ride.id.getId,
            arrival_time = arrivalTime
          }
buildOnUpdateMessage EstimateRepetitionBuildReq {..} = do
  let item = EstimateRepetitionOU.Item {id = estimateId.getId}
  return $
    OnUpdate.OnUpdateMessage $
      OnUpdate.EstimateRepetition
        EstimateRepetitionOU.EstimateRepetitionEvent
          { id = booking.id.getId,
            update_target = "state,fufillment.state.code",
            item = item,
            fulfillment = EstimateRepetitionOU.FulfillmentInfo ride.id.getId,
            cancellation_reason = castCancellationSource cancellationSource
          }

castCancellationSource :: SBCR.CancellationSource -> BookingCancelledOU.CancellationSource
castCancellationSource = \case
  SBCR.ByUser -> BookingCancelledOU.ByUser
  SBCR.ByDriver -> BookingCancelledOU.ByDriver
  SBCR.ByMerchant -> BookingCancelledOU.ByMerchant
  SBCR.ByAllocator -> BookingCancelledOU.ByAllocator
  SBCR.ByApplication -> BookingCancelledOU.ByApplication

castUpdateType :: UpdateType -> Maybe Bool
castUpdateType SIMPLE = Just False
castUpdateType FORCED = Just True
