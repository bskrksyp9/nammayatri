{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AccountSetUpScreen.ComponentConfig where

import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((/=), negate)
import PrestoDOM (Length(..), Margin(..))
import Screens.Types as ST
import Styles.Colors as Color
import Common.Types.App
import Animation.Config (AnimConfig, animConfig)
import PrestoDOM.Animation as PrestoAnim
import EN

primaryButtonConfig :: ST.AccountSetUpScreenState -> PrimaryButton.Config
primaryButtonConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig { text = (getString CONTINUE) }
        , isClickable = state.props.btnActive
        , alpha = if state.props.btnActive then 1.0 else 0.4
        , margin = (Margin 0 0 0 0)
        , enableLoader = (JB.getBtnLoader "AccountSetupScreen")
        , id = "AccountSetupScreen"
        , testIdText = (getEN CONTINUE)
        }
  in
    primaryButtonConfig'

primaryEditTextConfigName :: ST.AccountSetUpScreenState -> PrimaryEditText.Config
primaryEditTextConfigName state =
  let
    config = PrimaryEditText.config

    primaryEditTextConfig' =
      config
        { editText
          { color = Color.black800
          , placeholder = (getString ENTER_YOUR_NAME)
          , placeholderColor = Color.black600
          , singleLine = true
          , textSize = FontSize.a_16
          , fontStyle = if state.data.name /= "" then FontStyle.bold LanguageStyle else FontStyle.semiBold LanguageStyle
          , pattern = Just "[a-zA-Z ]*,30"
          }
        , background = Color.grey700
        , margin = (Margin 0 30 0 0)
        , topLabel
          { textSize = FontSize.a_12
          , text = (getString HOW_SHOULD_WE_ADDRESS_YOU)
          , color = Color.greyTextColor
          , fontStyle = FontStyle.regular LanguageStyle
          , alpha = 0.8
          }
        , testIdText = (getEN ENTER_YOUR_NAME)
        }
  in
    primaryEditTextConfig'

genericHeaderConfig :: GenericHeader.Config
genericHeaderConfig =
  let
    config = GenericHeader.config

    genericHeaderConfig' =
      config
        { height = WRAP_CONTENT
        , prefixImageConfig
          { height = V 25
          , width = V 25
          , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
          , margin = (Margin 12 12 12 12)
          }
        , background = Color.white900
        }
  in
    genericHeaderConfig'

goBackPopUpModelConfig :: PopUpModal.Config
goBackPopUpModelConfig =
  let
    config' = PopUpModal.config

    popUpConfig =
      config'
        { primaryText { text = (getString GO_BACK_) }
        , secondaryText { text = (getString REGISTER_USING_DIFFERENT_NUMBER) }
        , option1 { text = (getString NO), testIdText = (getEN NO)}
        , option2 { text = (getString YES), testIdText = (getEN YES)}
        }
  in
    popUpConfig

translateFullYAnimWithDurationConfigs :: ST.AccountSetUpScreenState -> AnimConfig
translateFullYAnimWithDurationConfigs state = animConfig {
  fromScaleY = if state.props.genderOptionExpanded then 0.0 else 1.0
, toScaleY =if state.props.genderOptionExpanded then 1.0 else 0.0
, fromY = if state.props.genderOptionExpanded then -100 else  0
, toY = if state.props.genderOptionExpanded then 0 else -100
, repeatCount = (PrestoAnim.Repeat 0)
, ifAnim = state.props.expandEnabled
, duration = 200
}