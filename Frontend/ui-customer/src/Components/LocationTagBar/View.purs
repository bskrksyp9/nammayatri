{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.LocationTagBar.View where

import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, color, cornerRadius, ellipsize, fontStyle, gravity, height, imageUrl, imageView, lineHeight, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, background, imageWithFallback)
import Components.LocationTagBar.Controller(Action(..))
import Data.Array (mapWithIndex, filter, findIndex, (!!), null)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Prelude (Unit, const, ($), (<>), (==))
import Styles.Colors as Color
import Screens.Types (LocationTagBarState, CardType(..), LocationListItemState)
import Language.Strings (getString)
import Data.Maybe (Maybe(..))
import Language.Types (STR(..))
import Engineering.Helpers.Commons(os)
import Common.Types.App
import Constant.Test as Id
import EN

view :: forall w. (Action -> Effect Unit) -> LocationTagBarState -> PrestoDOM ( Effect Unit ) w
view push state = 
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , orientation VERTICAL
 , Id.testId $ Id.Component Id.locationTagBar
 ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT 
    ](mapWithIndex (\index item -> 
        linearLayout
        [ height WRAP_CONTENT
        , stroke $ "1," <> Color.grey900
        , gravity CENTER
        , weight 1.0
        , background Color.white900
        , padding $ Padding 8 8 8 8
        , margin $ MarginRight if index == 2 then 0 else 8
        , onClick push $ const $ TagClick item (getSavedLocationByTag state item)
        , Id.testId $ Id.Object case item of
                                WORK_TAG -> getEN WORK
                                HOME_TAG -> getEN HOME
                                _        -> getEN ALL_FAVOURITES
        , cornerRadius 16.0
        ][ imageView
            [ width $ V 15
            , height $ V 17
            , imageWithFallback case item of
                        HOME_TAG -> if (getSavedLocationByTag state item) == Nothing then "ny_ic_add_address,https://assets.juspay.in/nammayatri/images/user/ny_ic_add_address.png" else "ny_ic_home_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_home_blue.png"
                        WORK_TAG -> if  (getSavedLocationByTag state item) == Nothing then "ny_ic_add_address,https://assets.juspay.in/nammayatri/images/user/ny_ic_add_address.png" else "ny_ic_work_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_work_blue.png"
                        _      -> "ny_ic_fav_red,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_red.png"
            ]
          , textView
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , textSize if os == "IOS" then FontSize.a_13 else FontSize.a_14
            , margin $ MarginLeft 8
            , color Color.black800
            , fontStyle $  FontStyle.medium LanguageStyle
            , gravity CENTER_VERTICAL
            , lineHeight "18"
            , padding $ PaddingBottom 1
            , ellipsize true
            , text case item of
                    WORK_TAG -> getString WORK
                    HOME_TAG -> getString HOME
                    _        -> getString ALL_FAVOURITES
            ]
            ]) [HOME_TAG, WORK_TAG, OTHER_TAG] )
    ]

getSavedLocationByTag :: LocationTagBarState -> CardType -> Maybe LocationListItemState
getSavedLocationByTag state tag = do 
  case (findIndex (\item -> item.tag == (getCard tag)) state.savedLocations) of
    Just index -> state.savedLocations !! index
    _          -> Nothing

getCard :: CardType -> String 
getCard cardType = case cardType of 
  HOME_TAG -> "Home"
  WORK_TAG -> "Work"
  _ -> ""
