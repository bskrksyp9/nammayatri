{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.PopUpModal.View where

import Prelude (Unit, const, unit, ($), (<>), (/), (-), (+), (==), (||), (&&), (>), not, (<<<), bind, discard, show, pure)
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), PrestoDOM, Visibility(..), afterRender, imageView, imageUrl, background, clickable, color, cornerRadius, fontStyle, gravity, height, linearLayout, margin, onClick, orientation, text, textSize, textView, width, stroke, alignParentBottom, relativeLayout, padding, visibility, onBackPressed, alpha, imageWithFallback, weight)
import Components.PopUpModal.Controller (Action(..), Config)
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Style as FontStyle
import Styles.Colors as Color
import Font.Size as FontSize
import Engineering.Helpers.Commons (screenHeight, screenWidth)
import PrestoDOM.Properties (cornerRadii)
import Common.Types.App
import Components.PrimaryEditText.View as PrimaryEditText
import Components.PrimaryEditText.Controller as PrimaryEditTextConfig
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (os, clearTimer, countDown)
import Data.Array ((!!), mapWithIndex)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Class (lift)
import JBridge (startTimerWithTime)
import Constant.Test as Id


view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , background Color.black9000
    , afterRender
        ( \action -> do
            _ <- push action
            if (state.option2.enableTimer || state.option1.enableTimer) then do
              let
                timerValue' = if state.option2.enableTimer then state.option2.timerValue else state.option1.timerValue
              if (os == "IOS") then
                liftEffect $ startTimerWithTime (show timerValue') "" "1" push CountDown
              else
                countDown timerValue' "" push CountDown
              pure unit
            else
              pure unit
        )
        (const NoAction)
    , onClick
        ( \action -> do
            _ <- push action
            clearTheTimer state
            pure unit
        )
        if (state.backgroundClickable && state.dismissPopup) then (const DismissPopup) else if state.backgroundClickable then (const OnButton1Click) else (const NoAction)
    , Id.testId $ Id.Component (if state.backgroundClickable then Id.back else Id.popUpModal)
    , gravity state.gravity
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , cornerRadii state.cornerRadius
        , orientation VERTICAL
        , background Color.white900
        , margin state.margin
        , padding state.padding
        , clickable true
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , visibility state.coverImageConfig.visibility
            , cornerRadii state.cornerRadius
            ]
            [ imageView
                [ height $ state.coverImageConfig.height
                , width $ state.coverImageConfig.width
                , margin $ state.coverImageConfig.margin
                , padding $ state.coverImageConfig.padding
                , imageWithFallback state.coverImageConfig.imageUrl
                , visibility state.coverImageConfig.visibility
                ]
            ]
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            ]
            [ textView
                [ text $ state.primaryText.text
                , textSize $ state.primaryText.fontSize
                , fontStyle $ state.primaryText.fontStyle
                , color $ state.primaryText.color
                , margin $ state.primaryText.margin
                , gravity $ state.primaryText.gravity
                , width if state.dismissPopupConfig.visibility == VISIBLE then WRAP_CONTENT else MATCH_PARENT
                , height WRAP_CONTENT
                , visibility $ state.primaryText.visibility
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity RIGHT
                , visibility state.dismissPopupConfig.visibility
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , margin state.dismissPopupConfig.margin
                    , onClick push $ const OnImageClick
                    , Id.testId $ Id.Object Id.close
                    , padding state.dismissPopupConfig.padding
                    ]
                    [ imageView
                        [ width state.dismissPopupConfig.width
                        , height state.dismissPopupConfig.height
                        , imageWithFallback state.dismissPopupConfig.imageUrl
                        , visibility state.dismissPopupConfig.visibility
                        ]
                    ]
                ]
            ]
        , textView
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , fontStyle $ FontStyle.medium LanguageStyle
            , color $ state.secondaryText.color
            , textSize FontSize.a_15
            , gravity $ state.secondaryText.gravity
            , padding state.secondaryText.padding
            , margin $ state.secondaryText.margin
            , text $ state.secondaryText.text
            , visibility $ state.secondaryText.visibility
            ]
        , contactView push state
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , visibility state.editTextVisibility
            ]
            [ PrimaryEditText.view (push <<< ETextController) (state.eTextConfig) ]
        , tipsView push state
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , orientation HORIZONTAL
            , margin state.buttonLayoutMargin
            ]
            [ linearLayout
                [ width $ if (state.optionButtonOrientation == "VERTICAL") then MATCH_PARENT else if ((not state.option1.visibility) || (not state.option2.visibility)) then MATCH_PARENT else WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation if (state.optionButtonOrientation == "VERTICAL") then VERTICAL else HORIZONTAL
                ]
                [ linearLayout
                    [ width if state.option2.visibility then (state.option1.width) else MATCH_PARENT
                    , background state.option1.background
                    , height $ V 48
                    , cornerRadius 8.0
                    , visibility $ if state.option1.visibility then VISIBLE else GONE
                    , stroke ("1," <> state.option1.strokeColor)
                    , clickable state.option1.isClickable
                    , alpha (if state.option1.isClickable then 1.0 else 0.5)
                    , margin $ state.option1.margin
                    , padding $ state.option1.padding
                    , gravity CENTER
                    , onClick
                        ( \action -> do
                            _ <- push action
                            clearTheTimer state
                            pure unit
                        )
                        (const OnButton1Click)
                    , Id.testId $ Id.Button $ Id.BtnConfig state.option1.testIdText
                    ]
                    [ textView
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , text $ if state.option1.enableTimer && state.option1.timerValue > 0 then (state.option1.text <> " (" <> (show state.option1.timerValue) <> ")") else state.option1.text
                        , color $ state.option1.color
                        , fontStyle $ state.option1.fontStyle
                        , textSize state.option1.fontSize
                        , gravity CENTER
                        ]
                    ]
                , linearLayout
                    [ width if state.option1.visibility then (state.option2.width) else MATCH_PARENT
                    , height $ V 48
                    , background $ state.option2.background
                    , cornerRadius 8.0
                    , visibility if state.option2.visibility then VISIBLE else GONE
                    , stroke ("1," <> state.option2.strokeColor)
                    , margin state.option2.margin
                    , gravity CENTER
                    , onClick
                        ( \action -> do
                            _ <- push action
                            clearTheTimer state
                            pure unit
                        )
                        (const OnButton2Click)
                    , Id.testId $ Id.Button $ Id.BtnConfig state.option2.testIdText
                    , padding state.option2.padding
                    , orientation VERTICAL
                    , clickable state.option2.isClickable
                    , alpha (if state.option2.isClickable then 1.0 else 0.5)
                    ]
                    [ textView
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , text $ if state.option2.enableTimer && state.option2.timerValue > 0 then (state.option2.text <> " (" <> (show state.option2.timerValue) <> ")") else state.option2.text
                        , color state.option2.color
                        , fontStyle $ state.option2.fontStyle
                        , textSize state.option2.fontSize
                        , gravity CENTER
                        ]
                    ]
                ]
            ]
        ]
    ]

tipsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
tipsView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , visibility if state.customerTipAvailable then VISIBLE else GONE
    , stroke $ "0," <> state.tipButton.strokeColor
    , margin state.tipLayoutMargin
    ]
    ( mapWithIndex
        ( \index item ->
            linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , weight 1.0
              , margin $ state.tipButton.margin
              ]
              [ textView
                  [ text $ ("" <> item)
                  , color $ state.tipButton.color
                  , textSize $ state.tipButton.fontSize
                  , visibility if state.tipButton.visibility then VISIBLE else GONE
                  , clickable $ state.tipButton.isClickable
                  , stroke $ "1," <> (if (state.activeIndex == index) then Color.blue800 else Color.grey900)
                  , cornerRadius 8.0
                  , width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , padding state.tipButton.padding
                  , fontStyle $ state.tipButton.fontStyle
                  , onClick push $ const $ Tipbtnclick index (fromMaybe 100 (state.customerTipArrayWithValues !! index))
                  , Id.testId $ Id.List (state.tipButton.testIdText)
                  , background (if (state.activeIndex == index) then Color.blue600 else state.tipButton.background)
                  ]
              ]
        )
        (state.customerTipArray)
    )

clearTheTimer :: Config -> Effect Unit
clearTheTimer config =
  if config.option1.enableTimer then do
    pure $ clearTimer config.option1.timerID
  else if config.option2.enableTimer then do
    pure $ clearTimer config.option2.timerID
  else
    pure unit

contactView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
contactView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin state.contactViewMargin
    , stroke ("1," <> Color.borderColorLight)
    , padding state.contactViewPadding
    , cornerRadius 8.0
    , visibility state.contactViewConfig.visibility
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity LEFT
        ]
        [ linearLayout
            [ height $ V 24
            , width $ V 24
            , background Color.yellow900
            , cornerRadius 12.0
            , gravity CENTER
            ]
            [ textView
                [ text state.contactViewConfig.nameInitials
                , color Color.black800
                , textSize FontSize.a_12
                ]
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , padding state.contactViewConfig.padding
            ]
            [ textView
                [ text state.contactViewConfig.fullName
                , color Color.black800
                , textSize FontSize.a_16
                , lineHeight "20"
                , fontStyle $ FontStyle.semiBold LanguageStyle
                ]
            ]
        ]
    ]