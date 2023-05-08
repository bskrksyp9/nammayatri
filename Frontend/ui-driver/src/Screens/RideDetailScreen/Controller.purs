{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideDetailScreen.Controller where

import Prelude(Unit, class Show, pure, unit, ($), (&&), bind, discard)
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import Screens.Types (RideDetailScreenState, Location)
import PrestoDOM.Types.Core (class Loggable)
import Data.Maybe (fromMaybe)
import Data.Number as Number
import Effect (Effect)
import JBridge (animateCamera, getCurrentPosition, isLocationEnabled, isLocationPermissionEnabled, showMarker, requestLocation, launchInAppRatingPopup)
import Effect.Timer (IntervalId, setInterval) as Timer
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, printLog, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen RIDE_DETAILS_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen RIDE_DETAILS_SCREEN)
      trackAppEndScreen appId (getScreen RIDE_DETAILS_SCREEN)
    MapSnapShot encImage -> do
      trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "in_screen" "map_snap_shot"
    GoToHome -> do
      trackAppActionClick appId (getScreen RIDE_DETAILS_SCREEN) "in_screen" "go_to_home"
      trackAppEndScreen appId (getScreen RIDE_DETAILS_SCREEN)
    NoAction -> trackAppScreenEvent appId (getScreen RIDE_DETAILS_SCREEN) "in_screen" "no_action"

data ScreenOutput = GoBack | GoToHomeScreen | ShowRoute
data Action = BackPressed
            | AfterRender
            | NoAction
            | GoToHome
            | MapSnapShot String

eval :: Action -> RideDetailScreenState -> Eval Action ScreenOutput RideDetailScreenState
eval AfterRender state = continue state
eval (MapSnapShot encImage) state = do
  _ <- pure $ printLog "encImage from rideDetail" encImage
  exit $ ShowRoute
eval GoToHome state = do
  _ <- pure $ launchInAppRatingPopup unit
  exit $ GoToHomeScreen
eval NoAction state = continue state
eval _ state = continue state

checkPermissionAndUpdateDriverMarker :: Effect Unit
checkPermissionAndUpdateDriverMarker = do
  conditionA <- isLocationPermissionEnabled unit
  conditionB <- isLocationEnabled unit
  if conditionA && conditionB then do
    _ <- getCurrentPosition (showDriverMarker "ny_ic_auto") constructLatLong
    pure unit
    else do
      _ <- requestLocation unit
      pure unit

showDriverMarker :: String -> Location -> Effect Unit
showDriverMarker marker location = do
  _ <- showMarker marker location.lat location.lon 100 0.5 0.5
  --_ <- showMarker "ic_active_marker" location.lat location.lng 350 0.5 0.5
  animateCamera location.lat location.lon 15

constructLatLong :: String -> String -> Location
constructLatLong lat lng =
  { lat: fromMaybe 0.0 (Number.fromString lat)
  , lon : fromMaybe 0.0 (Number.fromString lng)
  , place : ""
  }

