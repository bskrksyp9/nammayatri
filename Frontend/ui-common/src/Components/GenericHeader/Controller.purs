{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.GenericHeader.Controller where

import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), Orientation(..), padding)
import Styles.Colors as Color
import Common.Types.App
import Prelude ((<>))
import Helpers.Utils (getCommonAssetStoreLink)

data Action = SuffixImgOnClick | PrefixImgOnClick

type Config = 
  {
    height :: Length
  , width :: Length
  , margin :: Margin
  , padding :: Padding
  , background :: String
  , orientation :: Orientation
  , isClickable :: Boolean
  , gravity :: Gravity
  , prefixImageConfig :: ImageConfig
  , textConfig :: TextConfig 
  , suffixImageConfig :: ImageConfig
  }

type ImageConfig =
  {
    height :: Length
  , width :: Length
  , imageUrl :: String
  , margin :: Margin
  , padding :: Padding 
  , visibility :: Visibility
  }

type TextConfig =
  {
    text :: String
  , textSize :: Int
  , margin :: Margin
  , fontStyle :: String
  , color :: String
  }

config :: Config
config = {
    height : V 56
  , width : MATCH_PARENT
  , margin : (Margin 0 0 0 0)
  , padding : (Padding 0 5 0 5)
  , background : Color.white900
  , gravity : CENTER_VERTICAL
  , isClickable : true
  , orientation : HORIZONTAL
  , prefixImageConfig : {
      height : V 25
    , width : V 25
    , imageUrl : "ny_ic_chevron_left," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_left.png"
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 12 12 12 12)
    , visibility : VISIBLE
    }
  , textConfig : {
      text : ""
    , textSize : FontSize.a_18
    , margin : (Margin 0 0 0 0)
    , fontStyle : FontStyle.semiBold LanguageStyle
    , color : Color.black800
    }
  , suffixImageConfig : {
      height : V 25
    , width : V 25
    , imageUrl : ""
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , visibility : GONE
    }

}

merchantConfig :: Config
merchantConfig = {
    height : V 56
  , width : MATCH_PARENT
  , margin : (Margin 0 0 0 0)
  , padding : (Padding 0 0 0 0)
  , background : Color.white900
  , gravity : CENTER_VERTICAL
  , isClickable : true
  , orientation : VERTICAL
  , prefixImageConfig : {
      height : V 25
    , width : V 25
    , imageUrl : "ny_ic_chevron_left," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_left.png"
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 16 16 16 12)
    , visibility : VISIBLE
    }
  , textConfig : {
      text : ""
    , textSize : FontSize.a_24
    , margin : (Margin 16 0 16 10)
    , fontStyle : FontStyle.bold LanguageStyle
    , color : "#101010"
    }
  , suffixImageConfig : {
      height : V 25
    , width : V 25
    , imageUrl : ""
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , visibility : GONE
    }

}