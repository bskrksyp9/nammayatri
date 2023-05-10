{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.ErrorModal.View where
import Prelude (Unit, (<<<), ($), (<>), (==))
import Effect (Effect)
import Components.ErrorModal.Controller (Action(..), Config)
import PrestoDOM (Gravity(..), Length(..), Orientation(..), PrestoDOM, Padding(..), Margin(..), Visibility(..), color, fontStyle, gravity, height, linearLayout, margin, text, textSize, textView, width, visibility, orientation, imageView, imageUrl, padding, relativeLayout, alignParentBottom, background, stroke, clickable, imageWithFallback)
import Components.PrimaryButton.View as PrimaryButton
import Components.PrimaryButton.Controller as PrimaryButtonConfig
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import Constant.Test as Id

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , Id.testId $ Id.Component Id.errorModel
  ][  linearLayout[
        height config.height
      , width MATCH_PARENT
      , orientation VERTICAL
      , background config.background
      , cornerRadii config.corners
      , alignParentBottom "true,-1"
      , stroke config.stroke
      , gravity CENTER_VERTICAL
      , margin if (config.buttonConfig.visibility == VISIBLE) then (MarginBottom 50) else (MarginBottom 0)
      , clickable true
    ][errorView config]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , alignParentBottom "true,-1"
      , background config.background
      ][ PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig config)]
    ]
errorView :: forall w. Config -> PrestoDOM (Effect Unit) w
errorView config =
  linearLayout
  [ height config.height
  , width MATCH_PARENT
  , gravity CENTER
  , orientation VERTICAL
  ][  imageView
      [ imageWithFallback config.imageConfig.imageUrl
      , height config.imageConfig.height
      , width config.imageConfig.width
      , visibility config.imageConfig.visibility
      , margin config.imageConfig.margin
      ]
    , textView
      [ text config.errorConfig.text
      , textSize config.errorConfig.textSize
      , color config.errorConfig.color
      , fontStyle config.errorConfig.fontStyle
      , padding config.errorConfig.padding
      , gravity CENTER
      , margin config.errorConfig.margin
      , visibility config.errorConfig.visibility
      ]
    , textView
      [ text config.errorDescriptionConfig.text
      , textSize config.errorDescriptionConfig.textSize
      , color config.errorDescriptionConfig.color
      , fontStyle config.errorDescriptionConfig.fontStyle
      , padding config.errorDescriptionConfig.padding
      , margin config.errorDescriptionConfig.margin
      , gravity CENTER
      , visibility config.errorDescriptionConfig.visibility
      ]
  ]
primaryButtonConfig :: Config -> PrimaryButtonConfig.Config
primaryButtonConfig config = let
    config' = PrimaryButtonConfig.config
    primaryButtonConfig' = config'
      { textConfig
        { text = config.buttonConfig.text
        , fontStyle = config.buttonConfig.fontStyle
        , textSize = config.buttonConfig.textSize
        , color = config.buttonConfig.color
        }
      , width = config.buttonConfig.width
      , height = config.buttonConfig.height
      , cornerRadius = config.buttonConfig.cornerRadius
      , stroke = config.buttonConfig.stroke
      , background = config.buttonConfig.background
      , visibility = config.buttonConfig.visibility
      , margin = config.buttonConfig.margin
      , testIdText = config.testIdText
      }
  in primaryButtonConfig'