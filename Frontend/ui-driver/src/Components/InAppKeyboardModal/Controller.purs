{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.InAppKeyboardModal.Controller where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Prelude ((<>), class Eq)
import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), height, width)
import Styles.Colors as Color
import Common.Types.App(LazyCheck(..))
import Screens.Types(KeyboardModalType(..))

data Action = OnSelection String Int
            | OnClickDone String
            | AfterRender
            | OnClickBack String
            | OnclickTextBox Int
            | BackPressed
            | OnClickResendOtp
            | OnClickTextCross

----------------------------------------------- InAppKeyboardModalState ---------------------------------------------
type InAppKeyboardModalState = {
      errorConfig :: TextConfig
    , headingConfig :: TextConfig
    , subHeadingConfig :: TextConfig
    , inputTextConfig :: TextConfig
    , buttonConfig :: ButtonConfig
    , imageConfig :: ImageConfig
    , keyList :: Array Keys
    , otpIncorrect :: Boolean
    , otpAttemptsExceeded :: Boolean
    , modalType :: KeyboardModalType
    , isValidAlternateNumber :: Boolean
    , showResendOtpButton :: Boolean
}

type TextConfig =
  { text :: String
    , fontSize :: Int
    , focusIndex :: Int
    , fontStyle :: String
    , gravity :: Gravity
    , visibility :: Visibility
    , color :: String
    , height :: Length
    , width :: Length
    , cornerRadius :: Number
    , padding :: Padding
    , margin :: Margin
    , weight :: Number
  }

type ImageConfig =
  {
    imageUrl :: String
  , height :: Length
  , width :: Length
  , margin :: Margin
  , visibility :: Visibility
  , alpha :: Number
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

type Keys = {
  keys :: Array String
}

config :: InAppKeyboardModalState
config = {
    errorConfig : {
      text : ""
    , fontSize : FontSize.a_14
    , focusIndex : 0
    , fontStyle : FontStyle.medium LanguageStyle
    , gravity : CENTER
    , visibility : VISIBLE
    , color : Color.red
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , weight : 0.0
    },
    headingConfig : {
      text : ""
    , fontSize : FontSize.a_18
    , focusIndex : 0
    , fontStyle : FontStyle.semiBold LanguageStyle
    , gravity : CENTER
    , visibility : VISIBLE
    , color : Color.black800
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , margin : (MarginLeft 16)
    , weight : 0.0
    },
    subHeadingConfig : {
      text : ""
    , fontSize : FontSize.a_16
    , focusIndex : 0
    , fontStyle : FontStyle.medium LanguageStyle
    , gravity : CENTER
    , visibility : VISIBLE
    , color : Color.black900
    , height : WRAP_CONTENT
    , width : WRAP_CONTENT
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , weight : 500.0
    },
    inputTextConfig : {
       text : ""
    , fontSize : FontSize.a_16
    , focusIndex : 1
    , fontStyle : FontStyle.semiBold LanguageStyle
    , gravity : CENTER
    , visibility : VISIBLE
    , color : Color.black800
    , height : WRAP_CONTENT
    , width : MATCH_PARENT
    , cornerRadius : 0.0
    , padding : (Padding 0 0 0 0)
    , margin : (Margin 0 0 0 0)
    , weight : 1.0
    },
    buttonConfig : {
      margin : (Margin 0 0 0 0)
    , text : ""
    , fontStyle : FontStyle.bold LanguageStyle
    , textSize : FontSize.a_17
    , color : Color.yellow900
    , width : MATCH_PARENT
    , height : MATCH_PARENT
    , cornerRadius : 8.0
    , stroke : "0,#ffffff"
    , background : Color.black
    , visibility : VISIBLE
    },
    imageConfig : {
      imageUrl : ""
    , height : V 124
    , width : V 124
    , margin : (Margin 0 0 0 0)
    , visibility : VISIBLE
    , alpha : 1.0
    },
    keyList : [
    {
        keys: ["1", "2", "3"]
    },
    {
        keys: ["4", "5", "6"]
    },
    {
        keys: ["7", "8", "9"]
    },
    {
        keys: ["back", "0", "done"]
    }
    ]
  , otpIncorrect : false
  , otpAttemptsExceeded : false
  , modalType : NONE
  , isValidAlternateNumber : true
  , showResendOtpButton : false
  }
