{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.SaveFavouriteCard.View where

import Components.PrimaryEditText.Controller (Config, config) as PrimaryEditTextConfig
import Components.PrimaryEditText.View as PrimaryEditText
import Components.SaveFavouriteCard.Controller (Action(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, not, ($), (<<<), (<>), (&&), (/=), pure, bind, void)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, alpha, background, clickable, color, cornerRadius, ellipsize, fontStyle, gravity, height, imageUrl, imageView, lineHeight, linearLayout, margin, onClick, orientation, padding, singleLine, stroke, text, textSize, textView, weight, width, afterRender, adjustViewWithKeyboard, imageWithFallback)
import PrestoDOM.Animation as PrestoAnim
import JBridge (requestKeyboardShow)
import Screens.Types (SaveFavouriteCardState)
import Styles.Colors as Color
import Common.Types.App
import Constant.Test as Id
import EN

view :: forall w. (Action -> Effect Unit) -> SaveFavouriteCardState -> PrestoDOM ( Effect Unit ) w
view push state = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , background Color.blackLessTrans
  , gravity CENTER
  , padding (Padding 16 0 16 0)
  , clickable true
  , adjustViewWithKeyboard "true"
  , onClick push (const $ NoAction) 
  , Id.testId $ Id.Component Id.saveFavouriteCard
  , afterRender (\action -> void $ pure $ requestKeyboardShow (getNewIDWithTag "SaveFavouriteEditText")
      ) (const NoAction)
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , padding (Padding 16 24 16 16)
      , background Color.white900
      , clickable true
      , cornerRadius 15.0
      , gravity CENTER
      ][  titleView push state
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , background Color.white900
          , cornerRadius 4.0
          , gravity CENTER_VERTICAL
          , stroke $ "1,"<> Color.grey900
          , margin (Margin 0 16 0 16)
          ][  textView $
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , color Color.black600
              , text state.address
              , ellipsize true
              , singleLine true
              , padding (Padding 16 16 16 16)
              ] <> (FontStyle.body1 TypoGraphy)
          ]
        , PrimaryEditText.view (push <<< PrimayEditTA) (primaryEditTextConfig state)
        , textView $
          [ text $ (getString CONFIRM_AND_SAVE)
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , color Color.blue800
          , gravity CENTER
          , onClick push (const SaveFavourite)
          , Id.testId $ Id.Button $ Id.BtnConfig (getEN CONFIRM_AND_SAVE)
          , padding (Padding 20 14 20 14)
          , clickable ((not state.tagExists) && state.tag /= "" && (state.isBtnActive))
          , alpha if ((not state.tagExists) && state.tag /= "" && state.isBtnActive)  then 1.0 else 0.5
          ] <> FontStyle.subHeading1 TypoGraphy
        ]
    ]

titleView :: forall w. (Action -> Effect Unit) -> SaveFavouriteCardState -> PrestoDOM ( Effect Unit ) w
titleView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , orientation HORIZONTAL
  ][  textView
      [ text$ (getString SAVE) <> " " <> (getString FAVOURITE)
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      , textSize FontSize.a_18
      , color Color.black800
      , fontStyle $ FontStyle.bold LanguageStyle
      , gravity CENTER
      ]
    , linearLayout
      [ weight 1.0
      , orientation HORIZONTAL
      ][]
    , linearLayout[
        height WRAP_CONTENT
      , width WRAP_CONTENT
      , padding (Padding 16 8 0 8)
      ][imageView
      [ height $ V 24
      , width $ V 24
      , imageWithFallback "ny_ic_close,https://assets.juspay.in/nammayatri/images/common/ny_ic_close.png"
      , onClick push (const $ OnClose)
      , Id.testId $ Id.Object Id.closeIcon
      ]]
    ]

primaryEditTextConfig :: SaveFavouriteCardState -> PrimaryEditTextConfig.Config
primaryEditTextConfig state = 
  let config = PrimaryEditTextConfig.config
      config' = config  { 
        editText  { 
          color = Color.black800
        , singleLine = true
        , placeholder = (getString GIVE_THIS_LOCATION_A_NAME) 
        , fontStyle = FontStyle.medium LanguageStyle
        , textSize = FontSize.a_14
        , pattern = Just "[a-zA-Z0-9'‘’. ]*,30"
        , text = ""
        }
      , background = Color.white900
      , topLabel { 
          textSize = FontSize.a_12
        , text = (getString SAVE_AS)
        , color = Color.black800
        , fontStyle = FontStyle.medium LanguageStyle
        }
      , stroke = ("1,"<> Color.black500)
      , margin = (Margin 0 0 0 16)
      , id = (getNewIDWithTag "SaveFavouriteEditText")
      , errorLabel { 
          text = (getString NAME_ALREADY_IN_USE)
        , fontStyle = FontStyle.medium LanguageStyle
        , margin = (MarginVertical 4 4)
        }
      , showErrorLabel = state.tagExists
      }
    in config'
