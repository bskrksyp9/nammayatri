{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.BottomNavBar.View where

import Common.Types.App
import Components.BottomNavBar.Controller

import Data.Array (mapWithIndex)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (toString)
import JBridge (startLottieProcess)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, (==), const, (<>), (&&), bind, ($), pure, unit, (/=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Visibility(..), PrestoDOM, alignParentBottom, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, stroke, text, textSize, textView, weight, width, imageWithFallback, lottieAnimationView, id, afterRender, visibility)
import Screens.Types (BottomNavBarState)
import Storage (getValueToLocalNativeStore, KeyStore(..))
import Styles.Colors as Color
import Constant.Test as Id
import EN

view :: forall w . (Action -> Effect Unit) -> BottomNavBarState -> PrestoDOM (Effect Unit) w
view push state = 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , alignParentBottom "true,-1"
    , gravity CENTER
    , stroke ("1,"<> Color.borderColorLight)
    , Id.testId $ Id.Component Id.bottomNavBar
    ][ linearLayout
       [ width MATCH_PARENT
       , height MATCH_PARENT
       , margin (Margin 0 10 0 10)
       ](mapWithIndex 
         (\index item -> 
          linearLayout
          [ width WRAP_CONTENT
          , height MATCH_PARENT
          , weight 1.0
          , orientation VERTICAL
          , gravity CENTER
          , onClick push (const (OnNavigate item.text))
          , Id.testId $ Id.Bar case item.text of 
                      "Home"          -> getEN HOME
                      "Rides"         -> getEN RIDES
                      "Contest"       -> getEN CONTEST
                      "Profile"       -> getEN PROFILE
                      "Alert"         -> getEN ALERTS
                      _               -> Id.noAction
          ][ linearLayout
              [ width (V 24)
              , height (V 24)
              ][ if ((item.text == "Alert") && ((getValueToLocalNativeStore ALERT_RECEIVED) == "true") && state.activeIndex /= 3) then 
                    lottieLoaderView state push state.activeIndex item.text 
                 else
                    imageView 
                    [ width (V 24)
                    , height (V 24)
                    , imageWithFallback if state.activeIndex == index then item.activeIcon else item.defaultIcon
                    ] 
                ]
           , textView (
             [ width WRAP_CONTENT
             , height WRAP_CONTENT
             , color if index == state.activeIndex then Color.black else Color.black600
             , text case item.text of 
                      "Home"          -> getString HOME
                      "Rides"         -> getString RIDES
                      "Contest"       -> getString CONTEST
                      "Profile"       -> getString PROFILE
                      "Alert"         -> getString ALERTS
                      _               -> ""
             ] <> FontStyle.tags TypoGraphy)
           ]
         ) state.navButton
         )
    ]

lottieLoaderView :: forall w. BottomNavBarState -> (Action -> Effect Unit) -> Int -> String -> PrestoDOM (Effect Unit) w
lottieLoaderView state push activeIndex text =
  linearLayout
  [ width (V 25)
  , height (V 25)
  , visibility if((getValueToLocalNativeStore ALERT_RECEIVED) == "false" ) then GONE else VISIBLE
  ][ lottieAnimationView
    [ height (V 25)
    , width (V 25)
    , id (getIdForScreenIndex activeIndex)
    , afterRender
        ( \action -> do
            _ <- pure $ startLottieProcess "notification_bell" (getIdForScreenIndex activeIndex) true 1.0 "default"
            pure unit
        )
        (const OnNavigate text)
    ]
  ]

getIdForScreenIndex :: Int -> String
getIdForScreenIndex activeIndex = getNewIDWithTag ("NotificationBellAlertView" <> (toString activeIndex))