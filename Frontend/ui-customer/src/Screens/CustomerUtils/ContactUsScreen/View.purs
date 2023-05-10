{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ContactUsScreen.View where

import Animation as Anim 
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, unit, ($), (*), (-), (/), (<<<))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alignParentBottom, background, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, orientation, padding, relativeLayout, scrollView, text, textSize, textView, visibility, weight, width, imageWithFallback)
import Screens.ContactUsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
import Screens.CustomerUtils.ContactUsScreen.ComponentConfig 
import Constant.Test as Id

screen :: ST.ContactUsScreenState -> Screen Action ST.ContactUsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "ContactUsScreen"
  , globalEvents : []
  , eval
  }

view
  :: forall w
  . (Action -> Effect Unit) -> ST.ContactUsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $ linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , padding $ Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom
  , onBackPressed push $ const BackPressed
  , afterRender push (const AfterRender)
  , Id.testId $ Id.Screen Id.contactUsScreen
  ][  linearLayout[height WRAP_CONTENT
  , width WRAP_CONTENT
  , visibility if state.props.isSubmitted then GONE else VISIBLE][GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)]
    , instructionView state
    , relativeLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , visibility if state.props.isSubmitted then GONE else VISIBLE
      ][  editTextView state push
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , alignParentBottom "true,-1"
          , weight 1.0 
          ][PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfigSubmit state)]
        ]
    , afterSuccessfullSubmissionView state push
    ]
    
--------------------------- instructionView --------------------------- 
instructionView :: ST.ContactUsScreenState -> forall w . PrestoDOM (Effect Unit) w
instructionView state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , visibility if state.props.isSubmitted then GONE else VISIBLE
  , padding (Padding 16 12 16 12)
  , background Color.catskillWhite
  , orientation HORIZONTAL
  , gravity CENTER
  , visibility VISIBLE 
  ][  textView
      [ text (getString NOTE)
      , textSize FontSize.a_12
      , fontStyle $ FontStyle.medium LanguageStyle
      , color Color.black800
      ]
    , textView
      [ text (getString VISIT_MY_RIDES_SECTION_FOR_RIDE_SPECIFIC_COMPLAINTS)
      , textSize FontSize.a_12
      , fontStyle $ FontStyle.regular LanguageStyle
      , gravity CENTER
      , color Color.black800
      ]
    ]

--------------------------- editTextView --------------------------- 
editTextView :: ST.ContactUsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
editTextView state push =
  scrollView
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , padding (Padding 6 0 6 60)
      , orientation VERTICAL
      ][
          PrimaryEditText.view (push <<< SubjectEditTextActionController) (primaryEditTextConfig state)
        , PrimaryEditText.view (push <<< EmailEditTextActionController) (primaryEditTextConfigEmail state)
        , PrimaryEditText.view (push <<< DescriptionEditTextActionController) (primaryEditTextConfigDescription state)

      ]
      ]
    
--------------------------- afterSuccessfullSubmissionView --------------------------- 
afterSuccessfullSubmissionView :: ST.ContactUsScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
afterSuccessfullSubmissionView state push = 
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , visibility if state.props.isSubmitted then VISIBLE else GONE 
  ][ linearLayout[
    height $ V (4 * (EHC.screenHeight unit )/5)
    , width MATCH_PARENT
    , gravity CENTER
    , orientation VERTICAL
  ][ imageView
      [ imageWithFallback "ny_ic_letter,https://assets.juspay.in/nammayatri/images/user/ny_ic_letter.png"
      , height $ V 149
      , width $ V 149
      , margin (MarginBottom 32)
      ]
    , textView
      [ text (getString THANK_YOU_FOR_WRITING_TO_US)
      , textSize FontSize.a_22
      , fontStyle $ FontStyle.bold LanguageStyle
      , color Color.black900
      , margin (MarginBottom 12)
      ]
    , textView
      [ text (getString WE_HAVE_RECEIVED_YOUR_ISSUE_WELL_REACH_OUT_TO_YOU_IN_SOMETIME)
      , textSize FontSize.a_13
      , margin (Margin 42 0 42 0)
      , width $ V ((EHC.screenWidth unit - 84))
      , gravity CENTER
      , color Color.blackLightGrey
      ]]
    , linearLayout[
     width MATCH_PARENT
    , height MATCH_PARENT
    , gravity BOTTOM
    ][
      PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]
  ]
