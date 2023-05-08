{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.InAppKeyboardModal.View where

import Common.Types.App
import Components.InAppKeyboardModal.Controller (Action(..), InAppKeyboardModalState)
import Animation (translateYAnim)
import Animation.Config (translateYAnimConfig)
import Data.Array (mapWithIndex)
import Data.String (take, drop, length)
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Types (STR(..))
import Prelude (Unit, const, map, unit, ($), (/), (<>), (==), (||), (>=), (&&), (<), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), imageUrl, imageView, linearLayout, onBackPressed, onClick, textView, alpha)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (background, backgroundDrawable, clickable, color, cornerRadii, cornerRadius, fontStyle, gravity, height, imageUrl, margin, orientation, padding, stroke, text, textSize, weight, width, visibility,imageWithFallback)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import Debug (spy)
import Screens.Types(KeyboardModalType(..))
import Language.Strings (getString)

view :: forall w . (Action -> Effect Unit) -> InAppKeyboardModalState -> PrestoDOM (Effect Unit) w
view push state =
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , background Color.black9000
    , gravity BOTTOM
    ][
     PrestoAnim.animationSet [
        translateYAnim translateYAnimConfig
      ] $
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , cornerRadius 20.0
        , orientation VERTICAL
        , background Color.white900
        , gravity CENTER
        ][  linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER
            , margin (MarginTop 10)
            ][  linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , margin (Margin 20 20 20 20 )
                , gravity CENTER_VERTICAL
                ][  imageView
                    [ width (V 35)
                    , height (V 35)
                    , imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
                    , onClick push (const BackPressed)
                    , padding (Padding 5 5 5 5)
                    ]
                  , textView
                    [ width state.headingConfig.width
                    , height state.headingConfig.height
                    , gravity state.headingConfig.gravity
                    , text state.headingConfig.text
                    , color state.headingConfig.color
                    , textSize state.headingConfig.fontSize
                    , fontStyle state.headingConfig.fontStyle
                    , margin state.headingConfig.margin
                    , visibility state.headingConfig.visibility
                    , cornerRadius state.headingConfig.cornerRadius
                    , padding state.headingConfig.padding
                    , weight state.headingConfig.weight
                    ]

                ]
              , otpView push state
            ]
          , keyboard push state
        ]
    ]

textBoxes :: forall w . (Action -> Effect Unit) -> InAppKeyboardModalState -> PrestoDOM (Effect Unit) w
textBoxes push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , visibility if state.modalType == OTP && not state.otpAttemptsExceeded then VISIBLE else GONE
  , margin (Margin 0 20 0 20)
  , clickable false
  ](mapWithIndex (\index item ->
      textView
      [ width (V 48)
      , height (V 56)
      , color Color.greyTextColor
      , text ( take 1 (drop index state.inputTextConfig.text) )
      , textSize state.inputTextConfig.fontSize
      , fontStyle $ FontStyle.bold LanguageStyle
      , gravity CENTER
      , cornerRadius 4.0
      , stroke ("1," <> if (state.otpIncorrect || state.otpAttemptsExceeded ) then Color.textDanger else if state.inputTextConfig.focusIndex == index then Color.highlightBorderColor else Color.borderColorLight )
      , margin (Margin ((screenWidth unit)/30) 0 ((screenWidth unit)/30) 0)
      , onClick push (const (OnclickTextBox index))
      ]) [1,2,3,4])

singleTextBox :: forall w . (Action -> Effect Unit) -> InAppKeyboardModalState -> PrestoDOM (Effect Unit) w
singleTextBox push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , cornerRadius 4.0
  , visibility if state.modalType == MOBILE__NUMBER then VISIBLE else GONE
  , clickable false
  , padding (Padding 16 16 16 16)
  , stroke ("1," <> if (state.isValidAlternateNumber == false ) then Color.textDanger else Color.borderColorLight )
  ][textView
      [ width state.inputTextConfig.width
      , height state.inputTextConfig.height
      , color state.inputTextConfig.color
      , text state.inputTextConfig.text
      , textSize state.inputTextConfig.fontSize
      , fontStyle state.inputTextConfig.fontStyle
      , weight state.inputTextConfig.weight
      , gravity state.inputTextConfig.gravity
      , visibility state.inputTextConfig.visibility
      , cornerRadius state.inputTextConfig.cornerRadius
      , padding state.inputTextConfig.padding
      , margin state.inputTextConfig.margin
      , onClick push (const (OnclickTextBox 0))
      ],
    imageView
        [ width $ V 23
         , height $ V 23
         , imageWithFallback "ny_ic_close,https://assets.juspay.in/nammayatri/images/common/ny_ic_close.png"
         , visibility if (state.inputTextConfig.text == (getString ENTER_MOBILE_NUMBER)) then GONE else VISIBLE
         , onClick push (const (OnClickTextCross))
        ]
      ]

otpView :: forall w . (Action -> Effect Unit) -> InAppKeyboardModalState -> PrestoDOM (Effect Unit) w
otpView push state =
   linearLayout
      [ width MATCH_PARENT
           , height WRAP_CONTENT
           , margin (Margin 20 0 20 0)
           , orientation VERTICAL
           , gravity if(state.modalType == OTP) then CENTER else LEFT
       ]
             ([] <> [textBoxes push state] <> [singleTextBox push state] <>
                    [textView (
                    [ width state.subHeadingConfig.width
                    , height state.subHeadingConfig.height
                    , color state.subHeadingConfig.color
                    , text state.subHeadingConfig.text
                    , visibility state.subHeadingConfig.visibility
                    , fontStyle state.subHeadingConfig.fontStyle
                    , textSize state.subHeadingConfig.fontSize
                    , gravity state.subHeadingConfig.gravity
                    , cornerRadius state.subHeadingConfig.cornerRadius
                    , padding state.subHeadingConfig.padding
                    , margin state.subHeadingConfig.margin
                    , weight state.subHeadingConfig.weight
                    ]
                    )] <>
                    [textView (
                    [ width state.errorConfig.width
                    , height state.errorConfig.width
                    , visibility state.errorConfig.visibility
                    , margin state.errorConfig.margin
                    , text state.errorConfig.text
                    , color state.errorConfig.color
                    , textSize state.errorConfig.fontSize
                    , gravity state.errorConfig.gravity
                    , cornerRadius state.errorConfig.cornerRadius
                    , padding state.errorConfig.padding
                    , weight state.errorConfig.weight
                    ] <> FontStyle.body1 TypoGraphy
                  )] <>
                    [textView(
                      [
                        width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , text (getString RESEND_OTP)
                      , textSize FontSize.a_12
                      , fontStyle $ FontStyle.medium LanguageStyle
                      , color Color.blue900
                      , margin (Margin 0 0 0 0)
                      , onClick push (const (OnClickResendOtp))
                      , visibility if (state.modalType == OTP && state.showResendOtpButton && (not state.otpAttemptsExceeded)) then VISIBLE else GONE
                      ]
                    )])


keyboard :: forall w . (Action -> Effect Unit) -> InAppKeyboardModalState -> PrestoDOM (Effect Unit) w
keyboard push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginTop 40
  , padding (Padding 0 5 0 20)
  , gravity CENTER
  , background Color.grey800
  ] (map (\(item) ->
    linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation HORIZONTAL
     , margin (Margin 4 0 4 0)
     , gravity CENTER
     ] (mapWithIndex (\index key ->
       linearLayout
       [ width MATCH_PARENT
       , height WRAP_CONTENT
       , gravity CENTER
       , weight 1.0
       , backgroundDrawable "button"
       ][  if (key == "back" || key == "done") then
           linearLayout
           [ width MATCH_PARENT
           , height WRAP_CONTENT
           , gravity CENTER
           , margin (Margin 3 3 3 3)
           , alpha if (key == "done") then state.imageConfig.alpha else 1.0
           , background if key == "back" then Color.lightGrey else Color.darkMint
           , cornerRadius 4.0
           , cornerRadii $ if key == "back" then Corners 30.0 false false false true else Corners 30.0 false false true false
           , onClick push if key == "back" then (const (OnClickBack state.inputTextConfig.text)) else (const (OnClickDone state.inputTextConfig.text))
           , clickable if key == "back" then true else
                      if ((length state.inputTextConfig.text == 4  && state.modalType == OTP && state.otpIncorrect == false) || (length state.inputTextConfig.text == 10  && state.modalType == MOBILE__NUMBER && state.isValidAlternateNumber==true)) then true else false
           ][
                if key == "back" then
                imageView
                  [ width $ V 24
                  , height $ V 24
                  , imageWithFallback "ny_ic_delete,https://assets.juspay.in/nammayatri/images/common/ny_ic_delete.png"
                  , margin (Margin 0 18 0 18)
                  ]
                else
                  imageView
                  [ width $ V 24
                  , height $ V 24
                  , imageWithFallback "ny_ic_tick_white,https://assets.juspay.in/nammayatri/images/common/ny_ic_tick_white.png"
                  , margin (Margin 0 18 0 18)
                  ]
           ]
           else
           linearLayout
           [ width MATCH_PARENT
           , height WRAP_CONTENT
           , gravity CENTER
           , margin (Margin 3 3 3 3)
           , background Color.white900
           , cornerRadius 4.0
           , onClick push (const (OnSelection key state.inputTextConfig.focusIndex))
           ][  textView
               [ width WRAP_CONTENT
               , height MATCH_PARENT
               , text key
               , color Color.greyTextColor
               , textSize FontSize.a_21
               , fontStyle $ FontStyle.bold LanguageStyle
               , padding (Padding 0 15 0 15)
               ]
           ]
       ]
       ) item.keys )
    ) state.keyList )
