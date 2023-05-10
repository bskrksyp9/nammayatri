{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectLanguageScreen.View where

import Prelude (Unit, const, ($), (<<<), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, alpha, background, color, fontStyle, gravity, height, imageUrl, imageView, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, text, textSize, textView, weight, width, afterRender, imageWithFallback)
import Effect (Effect)
import Screens.SelectLanguageScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Animation as Anim
import Components.SelectMenuButton.View as MenuButton
import Language.Strings (getString)
import Language.Types (STR(..))
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Common.Types.App
import Screens.SelectLanguageScreen.ComponentConfig
import Constant.Test as Id

screen :: ST.SelectLanguageScreenState -> Screen Action ST.SelectLanguageScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "SelectLanguageScreen"
  , globalEvents : []
  , eval
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.SelectLanguageScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , onBackPressed push (const BackPressed)
      , background Color.white900
      , afterRender push (const AfterRender)
      , Id.testId $ Id.Screen Id.selectLanguageScreen
      ][  headerLayout push state
        , menuButtonsView state push
        , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
      ]

-------------------------------------------------- headerLayout --------------------------
headerLayout :: (Action -> Effect Unit) -> ST.SelectLanguageScreenState ->  forall w . PrestoDOM (Effect Unit) w
headerLayout push state = 
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , orientation VERTICAL
 ][ linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation HORIZONTAL
    , layoutGravity "center_vertical"
    , padding (Padding 5 5 5 0)
    ][ imageView
        [ width $ V 25
        , height MATCH_PARENT
        , imageWithFallback "ny_ic_back,https://assets.juspay.in/nammayatri/images/driver/ny_ic_back.png"
        , gravity CENTER_VERTICAL
        , onClick push (const BackPressed)
        , Id.testId $ Id.ToolBar Id.backIcon
        , padding (Padding 2 2 2 2)
        , margin (MarginLeft 5)
        ]
      , textView
        [ width WRAP_CONTENT
        , height MATCH_PARENT
        , text (getString SELECT_LANGUAGE)
        , textSize FontSize.a_19
        , margin (MarginLeft 20)
        , color Color.black
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , weight 1.0
        , gravity CENTER_VERTICAL
        ]
    ]
  , linearLayout
    [ width MATCH_PARENT
    , height $ V 1 
    , background Color.black800
    , alpha 0.1
    ][]
 ]

------------------------------ menuButtonsView ------------------------------
menuButtonsView :: ST.SelectLanguageScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
menuButtonsView state push = 
 scrollView
  [ width MATCH_PARENT
  , weight 1.0
  , margin (MarginTop 15)
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ Margin 1 0 1 0
      , background Color.white900
      ](DA.mapWithIndex
          (\ index language ->
          MenuButton.view
              (push <<< (MenuButtonAction))
              { text: {name: language.name, value: language.value, subtitle: language.subtitle}, isSelected: (state.props.selectedLanguage == language.value), index : index }) state.data.languages
      )
  ]