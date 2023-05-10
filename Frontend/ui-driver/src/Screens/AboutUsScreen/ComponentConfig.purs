{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AboutUsScreen.ComponentConfig where

import Common.Types.App
import Components.PopUpModal as PopUpModal
import Data.Maybe
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import PrestoDOM.Types.DomAttributes as PTD
import Screens.Types as ST
import Styles.Colors as Color
import EN

demoModePopUpConfig :: ST.AboutUsScreenState -> PopUpModal.Config
demoModePopUpConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    gravity = CENTER,
    margin = (MarginHorizontal 16 16),
    buttonLayoutMargin = (Margin 0 16 16 0),
    editTextVisibility = VISIBLE,
    dismissPopupConfig {
      visibility = VISIBLE, 
      height = V 12,
      width = V 12,
      margin = (Margin 0 26 22 0)
      },
    eTextConfig { 
      editText{placeholder = (getString ENTER_DEMO_MODE_PASSWORD),
      fontStyle = FontStyle.medium LanguageStyle,
      textSize = FontSize.a_14,
      pattern = Just "[^\n]*,7"
      },
    topLabel { 
      text = (getString PASSWORD),
      color = Color.black900
      }, 
      margin = (Margin 16 16 16 0), type = "number"
    },
    primaryText {
      text = (getString DEMO_MODE), 
      gravity = LEFT,
      margin = (Margin 16 21 0 0)
      },
    secondaryText { 
      visibility = GONE
      },
    option1 {
      visibility = false
      },
    option2 { 
      text = (getString CONFIRM_PASSWORD),
      background = Color.white900, 
      color=Color.blue800, 
      strokeColor = Color.white900, 
      padding = (Padding 16 0 16 0), 
      fontSize = FontSize.a_16
    , isClickable = state.props.enableConfirmPassword
    , testIdText = (getEN CONFIRM_PASSWORD)
    },
    cornerRadius = (PTD.Corners 15.0 true true true true)
  }
  in popUpConfig'