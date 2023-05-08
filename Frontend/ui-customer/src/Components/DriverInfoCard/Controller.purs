{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.DriverInfoCard.Controller where

import Components.PrimaryButton as PrimaryButtonController
import Components.SourceToDestination as SourceToDestinationController
import Screens.Types(Stage)

data Action = NoAction
            | Support 
            | PrimaryButtonAC PrimaryButtonController.Action
            | SourceToDestinationAC SourceToDestinationController.Action
            | CancelRide DriverInfoCardState
            | LocationTracking
            | OpenEmergencyHelp
            | MessageDriver
            | ShareRide

type DriverInfoCardState = 
  { props :: DriverInfoCardProps
  , data :: DriverInfoCardData
  }

type DriverInfoCardProps = 
  {
    currentStage :: Stage,
    trackingEnabled :: Boolean,
    unReadMessages :: Boolean
  }

type DriverInfoCardData = 
  { otp :: String
  , driverName :: String
  , eta :: Int
  , vehicleDetails :: String 
  , registrationNumber :: String 
  , rating :: Number
  , startedAt :: String 
  , endedAt :: String
  , source :: String 
  , destination :: String
  , rideId :: String
  , price :: Int
  , sourceLat :: Number
  , sourceLng :: Number
  , destinationLat :: Number
  , destinationLng :: Number
  , driverLat :: Number
  , driverLng :: Number
  , distance :: Int
  , waitingTime :: String
  , driverArrived :: Boolean
  , estimatedDistance :: String
  , driverArrivalTime :: Int
  , bppRideId :: String
  }
