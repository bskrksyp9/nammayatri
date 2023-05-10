{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.UploadDrivingLicenseScreen.ComponentConfig where

import Common.Types.App
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings
import Language.Types (STR(..))
import Screens.Types as ST
import Styles.Colors as Color
import Prelude
import PrestoDOM
import EN

------------------------------ primaryButtonConfig --------------------------------
primaryButtonConfig :: ST.UploadDrivingLicenseState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString NEXT), textSize = FontSize.a_16}
      , width = MATCH_PARENT
      , background = Color.black900
      , margin = (Margin 0 0 0 0)
      , cornerRadius = 0.0
      , height = (V 60)
      , isClickable = state.data.imageFront /= "" && state.data.dob /= "" && DS.length state.data.driver_license_number >= 9 && (DS.toLower(state.data.driver_license_number) == DS.toLower(state.data.reEnterDriverLicenseNumber)) && (state.data.dateOfIssue /= Just "")
      , alpha = if (state.data.imageFront /= "" && state.data.dob /= "" && DS.length state.data.driver_license_number >= 9) && (DS.toLower(state.data.driver_license_number) == DS.toLower(state.data.reEnterDriverLicenseNumber)) && (state.data.dateOfIssue /= Just "") then 1.0 else 0.8
      , testIdText = (getEN NEXT)
      }
  in primaryButtonConfig'

------------------------------ primaryEditTextConfig --------------------------------
primaryEditTextConfig :: ST.UploadDrivingLicenseState -> PrimaryEditText.Config
primaryEditTextConfig state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { singleLine = true
          , pattern = Just "[A-Z0-9]*,16"
          , fontStyle = FontStyle.bold LanguageStyle
          , textSize = FontSize.a_16
          , placeholder = (getString ENTER_DL_NUMBER)
          , capsLock = true
        }
      , topLabel
        { textSize = FontSize.a_12
        , text = (getString DRIVING_LICENSE_NUMBER)
        , color = Color.greyTextColor
        }
      , margin = (MarginBottom 15)
      , background = Color.white900
      , id = (EHC.getNewIDWithTag "EnterDrivingLicenseEditText")
      , testIdText = (getEN ENTER_DL_NUMBER)
      }
    in primaryEditTextConfig'

------------------------------ primaryEditTextConfigReEnterDl --------------------------------
primaryEditTextConfigReEnterDl :: ST.UploadDrivingLicenseState -> PrimaryEditText.Config
primaryEditTextConfigReEnterDl state = let 
    config = PrimaryEditText.config
    primaryEditTextConfig' = config
      { editText
        { singleLine = true
          , pattern = Just "[A-Z0-9]*,16"
          , fontStyle = FontStyle.bold LanguageStyle
          , textSize = FontSize.a_16
          , placeholder = (getString ENTER_DL_NUMBER)
          , capsLock = true
          , color = Color.black800
        }
      , stroke = if (DS.toLower(state.data.driver_license_number) /= DS.toLower(state.data.reEnterDriverLicenseNumber)) then ("1," <> Color.red) else ("1," <> Color.borderColorLight)
      , topLabel
        { textSize = FontSize.a_12
        , text = (getString RE_ENTER_DRIVING_LICENSE_NUMBER)
        , color = Color.greyTextColor
        }
      , margin = (MarginBottom 15)
      , background = Color.white900
      , id = (EHC.getNewIDWithTag "ReEnterDrivingLicenseEditText")
      , testIdText = (getEN RE_ENTER_DRIVING_LICENSE_NUMBER)
      }
    in primaryEditTextConfig'