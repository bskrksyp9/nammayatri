{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.MyRidesScreen.ComponentConfig where

import Common.Types.App
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST
import Styles.Colors as Color
import Storage as Storage
import EN

apiErrorModalConfig :: ErrorModal.Config 
apiErrorModalConfig = let 
  config = ErrorModal.config 
  errorModalConfig' = config 
    { imageConfig {
        imageUrl = "ny_ic_error_404,https://assets.juspay.in/nammayatri/images/user/ny_ic_error_404.png"
      , height = V 110
      , width = V 124
      , margin = (MarginBottom 32)
      }
    , errorConfig {
        text = (getString ERROR_404)
      , margin = (MarginBottom 7)  
      , color = Color.black800
      , textSize = FontSize.a_18
      , fontStyle = FontStyle.bold LanguageStyle
      }
    , errorDescriptionConfig {
        text = (getString PROBLEM_AT_OUR_END)
      , color = Color.black700
      , textSize = FontSize.a_14
      , margin = (Margin 16 0 16 0)
      , fontStyle =  FontStyle.regular LanguageStyle
      }
    , buttonConfig {
        text = (getString NOTIFY_ME)
      , margin = (Margin 16 0 16 16)
      , background = Color.black900
      , color = Color.yellow900
      , fontStyle = FontStyle.medium LanguageStyle
      , textSize = FontSize.a_16
      }
    , testIdText = (getEN NOTIFY_ME)
    }
  in errorModalConfig' 

errorModalConfig :: ST.MyRidesScreenState -> ErrorModal.Config 
errorModalConfig state = let 
  config = ErrorModal.config 
  errorModalConfig' = config 
    { imageConfig {
        imageUrl = "ny_ic_no_past_rides,https://assets.juspay.in/nammayatri/images/common/ny_ic_no_past_rides.png"
      , height = V 110
      , width = V 124
      , margin = (MarginBottom 32)
      }
    , errorConfig {
        text = (getString EMPTY_RIDES)
      , margin = (MarginBottom 7)  
      , color = Color.black800
      , textSize = FontSize.a_18
      , fontStyle = FontStyle.bold LanguageStyle
      }
    , errorDescriptionConfig {
        text = (getString YOU_HAVENT_TAKEN_A_TRIP_YET)
      , color = Color.black700
      , textSize = FontSize.a_14
      , fontStyle =  FontStyle.regular LanguageStyle
      }
    , buttonConfig {
        text = (getString BOOK_NOW)
      , margin = (Margin 16 0 16 16)
      , background = Color.black900
      , color = Color.yellow900
      , fontStyle = FontStyle.medium LanguageStyle
      , textSize = FontSize.a_16
      , visibility = if (Storage.isLocalStageOn ST.HomeScreen) then VISIBLE else GONE
      }
    , testIdText = (getEN BOOK_NOW)
    }
  in errorModalConfig' 

genericHeaderConfig :: ST.MyRidesScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , margin = (Margin 12 12 12 12)
      } 
    , textConfig {
        text = (getString MY_RIDES)
      , textSize = FontSize.a_18
      , color = Color.darkDescriptionText
      , fontStyle = FontStyle.bold LanguageStyle
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = (Padding 0 5 0 5)
    }
  in genericHeaderConfig'