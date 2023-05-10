{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.ErrorModal.Controller where

import PrestoDOM ( Length(..), Margin(..), Visibility(..), Padding(..), Gravity(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Components.PrimaryButton.Controller as PrimaryButtonController 
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import Common.Types.App

data Action = PrimaryButtonActionController PrimaryButtonController.Action 

type Config =
  { 
    height :: Length
  , gravity :: Gravity
  , background :: String 
  , stroke :: String 
  , corners :: Corners
  , imageConfig :: ImageConfig
  , errorConfig :: TextConfig
  , errorDescriptionConfig :: TextConfig
  , buttonConfig :: ButtonConfig 
  , testIdText :: String
  }

type ImageConfig = 
  { imageUrl :: String
  , height :: Length
  , width :: Length 
  , margin :: Margin 
  , visibility :: Visibility
  }

type TextConfig =
  { text :: String
  , textSize :: Int
  , fontStyle :: String
  , color :: String 
  , padding :: Padding
  , margin :: Margin 
  , visibility :: Visibility
  }

type ButtonConfig =
  { margin :: Margin 
  , text :: String
  , fontStyle :: String
  , textSize :: Int
  , color :: String 
  , width :: Length
  , height :: Length
  , cornerRadius :: Number 
  , stroke :: String
  , background :: String 
  , visibility :: Visibility
  }

config :: Config 
config = 
  { height : MATCH_PARENT
  , gravity : CENTER 
  , background : Color.white900 
  , corners : (Corners 0.0 false false false false)
  , stroke : "0,#000000"
  , imageConfig : 
    { imageUrl : ""
    , height : V 124
    , width : V 124
    , margin : (Margin 0 0 0 0)
    , visibility : VISIBLE
    }
  , errorConfig :
    { text : ""
    , textSize : FontSize.a_18
    , fontStyle : FontStyle.medium LanguageStyle
    , color : Color.black
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , visibility : VISIBLE
    }
  , errorDescriptionConfig :
    { text : ""
    , textSize : FontSize.a_14 
    , fontStyle : FontStyle.regular LanguageStyle
    , color : Color.black
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , visibility : VISIBLE
    }
  , buttonConfig :
    { margin : (Margin 0 0 0 0)
    , text : ""
    , fontStyle : FontStyle.bold LanguageStyle
    , textSize : FontSize.a_17
    , color : Color.yellow900
    , width : MATCH_PARENT
    , height : V 50
    , cornerRadius : 8.0
    , stroke : "0,#ffffff"
    , background : Color.black
    , visibility : VISIBLE
    }
  , testIdText : ""

  }