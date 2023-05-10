{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.ChatView.View where
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), scrollBarY, alignParentBottom, background, color, cornerRadius, fontStyle, gravity, height, id, imageView, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width, editText, onChange, hint, scrollView, onAnimationEnd, pattern, ellipsize, clickable, singleLine, maxLines, hintColor, imageWithFallback)
import Engineering.Helpers.Commons (getNewIDWithTag, screenWidth, os)
import Animation (translateInXForwardAnim, translateInXBackwardAnim)
import PrestoDOM.Animation as PrestoAnim
import Prelude (Unit, bind, const, pure, unit, ($), (&&), (-), (/), (<>), (==), (>), (*))
import PrestoDOM.Properties (cornerRadii, lineHeight)
import Font.Size as FontSize
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Style as FontStyle
import Data.Array (mapWithIndex , (!!), length, null)
import Data.String (split, Pattern(..), length) as STR
import Data.Maybe (fromMaybe, Maybe(..))
import JBridge (scrollToBottom)
import Components.ChatView.Controller (Action(..), Config(..), ChatComponent)
import Common.Types.App
import Constant.Test as Id

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
view push config = 
  linearLayout
  [ height $ V 396
  , width MATCH_PARENT
  , orientation VERTICAL
  , alignParentBottom "true,-1"
  , clickable true
  , cornerRadii $ Corners 24.0 true true false false 
  , gravity BOTTOM
  , stroke ("1," <> config.grey800)
  , background config.white900
  , Id.testId $ Id.Component Id.chatView
  ]
  [ chatHeaderView config push
  , chatBodyView config push
  , chatFooterView config push
  ]
chatHeaderView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatHeaderView config push =
  linearLayout
  [ orientation VERTICAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER_VERTICAL
      , orientation HORIZONTAL
      , padding (PaddingHorizontal 8 16)
      , margin (Margin 0 16 0 16)
      ][ linearLayout
         [ height $ V 40
         , width $ V 40
         , gravity CENTER
         , onClick push (const BackPressed)
         , Id.testId $ Id.Object Id.backIcon
         ][ imageView
            [ imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
            , height $ V 24
            , width $ V 24
            ]
          ]
        , headerNameView config push
        , headerActionView config push
        ]
    , linearLayout
      [ width MATCH_PARENT
      , height $ V 1
      , background config.grey900
      ][]
  ]

headerNameView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerNameView config push = 
  linearLayout
  [ height WRAP_CONTENT
  , width (V (((screenWidth unit)/100)* (getConfig config.userConfig.appType).margin )) 
  , orientation VERTICAL
  ][textView 
    [ text config.userConfig.userName
    , textSize FontSize.a_16
    , color config.black800
    , fontStyle $ FontStyle.semiBold LanguageStyle
    , ellipsize true
    , singleLine true
    ]
   ,textView
    [ text config.distance
    , textSize FontSize.a_12
    , visibility (getConfig config.userConfig.appType).customerVisibility
    , color config.black700
    , fontStyle $ FontStyle.regular LanguageStyle
    , ellipsize true
    , singleLine true
    ]
  ]

headerActionView ::forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerActionView config push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity RIGHT
  ][ linearLayout
     [ height $ V 40
     , width $ V 68
     , visibility (getConfig config.userConfig.appType).customerVisibility
     , gravity CENTER
     , cornerRadius if os == "IOS" then 20.0 else 32.0
     , clickable true
     , background config.green200
     , onClick push (const Call)
     , Id.testId $ Id.Object Id.call
     ][ imageView
        [ imageWithFallback "ny_ic_call,https://assets.juspay.in/nammayatri/images/common/ny_ic_call.png"
        , height $ V 18
        , width $ V 18
        ]
     ]
  , linearLayout
    [ height $ V 40
    , width $ V 60
    , visibility (getConfig config.userConfig.appType).driverVisibility
    , gravity CENTER
    , background config.grey700
    , stroke $ "1,"<> config.grey900
    , cornerRadius 32.0
    , margin $ MarginRight 8
    , onClick push (const $ Call)
    , Id.testId $ Id.Object Id.call
    ][ imageView
       [ imageWithFallback "ic_phone,https://assets.juspay.in/nammayatri/images/common/ic_phone.png"
       , height $ V 20
       , width $ V 20
       ]
    ]
  , linearLayout
    [ height $ V 40
    , width $ V 94
    , visibility (getConfig config.userConfig.appType).driverVisibility
    , gravity CENTER
    , orientation HORIZONTAL
    , background config.blue600
    , stroke $ "1,"<> config.blue900
    , cornerRadius 32.0
    , onClick push (const $ Navigate)
    , Id.testId $ Id.Object Id.navigate
    ][ imageView
       [ imageWithFallback "ic_navigation_blue,https://assets.juspay.in/nammayatri/images/common/ic_navigation_blue.png"
       , height $ V 20
       , width $ V 20
       , margin $ MarginRight 8
       ]
     , textView
       [ text config.mapsText
       , color config.blue900
       , textSize FontSize.a_16
       , fontStyle $ FontStyle.medium LanguageStyle
       ]
    ]  
  ]

chatBodyView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatBodyView config push =
  if (length config.messages) == 0 then 
    emptyChatView config push
  else 
    chatView config push
  
chatView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatView config push =
  linearLayout
  [ height $ V 224
  , width MATCH_PARENT
  , orientation VERTICAL
  ] ([ scrollView
      [ height $ V 224
      , width MATCH_PARENT
      , id (getNewIDWithTag "ChatScrollView")
      , scrollBarY false
      ] 
      [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][linearLayout
         [ height WRAP_CONTENT
         , width MATCH_PARENT
         , orientation VERTICAL 
         , padding (PaddingHorizontal 16 16)
         ](mapWithIndex (\index item -> chatComponent config item (index == (length config.messages - 1)) (config.userConfig.appType)) (config.messages))
       , suggestionsView config push
        ]
      ]
    ])

chatFooterView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
chatFooterView config push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , alignParentBottom "true,-1"
  , background config.white900
  ][ linearLayout
     [ width (V (screenWidth unit))
     , height $ V 1
     , background config.grey900
     , margin $ MarginTop 16
     ][]
    , linearLayout 
      [ height $ V 48
      , width MATCH_PARENT
      , padding (PaddingHorizontal 16 8)
      , margin (Margin 16 16 16 16)
      , cornerRadius 24.0
      , gravity CENTER_VERTICAL
      , background config.grey800
      , orientation HORIZONTAL
      ][ editText
         [ weight 1.0
         , height $ V 48 
         , id (getNewIDWithTag "ChatInputEditText")
         , background config.grey800
         , cornerRadius 24.0
         , textSize FontSize.a_14
         , hint $ config.hint <> " " <> fromMaybe "" ((STR.split (STR.Pattern " ") config.userConfig.userName) !! 0) <> "..."
         , singleLine true
         , hintColor config.black700
         , ellipsize true
         , fontStyle $ FontStyle.medium LanguageStyle
         , onChange push $ TextChanged
         , Id.testId $ Id.TextField Id.chatInput
         , pattern "[^\n]*,255"
         ]
       , linearLayout
         [ height $ V 36
         , width $ V 36
         , gravity CENTER
         , onClick push (const (SendMessage))
         , Id.testId $ Id.Object Id.send
         ][ imageView
            [ imageWithFallback if config.sendMessageActive then "ic_send_blue,https://assets.juspay.in/nammayatri/images/common/ic_send_blue.png" else "ic_send,https://assets.juspay.in/nammayatri/images/common/ic_send.png"
            , height $ V 20 
            , width $ V 20 
            ] 
         ]
      ]
  ]  

emptyChatView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
emptyChatView config push =
  linearLayout
  [ height $ V 224
  , width MATCH_PARENT
  ] 
  [ linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , orientation VERTICAL
     , background config.white900
     ]([ textView 
       [ text $ if config.userConfig.appType == "Customer" && null config.suggestionsList && null config.messages then config.emptyChatHeader else config.suggestionHeader
       , color config.black700
       , textSize FontSize.a_14
       , width MATCH_PARENT
       , margin (Margin 16 16 16 20)
       , maxLines 2
       , ellipsize true
       , gravity CENTER
       , fontStyle $ FontStyle.medium LanguageStyle
       ]
     ] <> [suggestionsView config push])
  ] 
  
suggestionsView :: forall w. Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
suggestionsView config push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity RIGHT
  , margin (Margin 0 4 16 0)
  , visibility VISIBLE
  ][ linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity LEFT
    , stroke ("1,"<> config.grey900)
    , cornerRadius 8.0
    , orientation VERTICAL
    ] (mapWithIndex (\index item -> quickMessageView config item (if index == (length config.suggestionsList-1) then true else false) push) (config.suggestionsList))
  ]
quickMessageView :: forall w. Config -> String -> Boolean -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
quickMessageView config message isLastItem push =
  linearLayout
  [ height WRAP_CONTENT
  , width (V (((screenWidth unit)/100)* 80))
  , gravity LEFT
  , orientation VERTICAL
  , onClick push (const (SendSuggestion message))
  , Id.testId $ Id.Container Id.send
  ][ textView
     [ text (message)
     , color config.blue800
     , padding (Padding 12 16 12 16)
     , textSize FontSize.a_14
     , lineHeight "18"
     , fontStyle $ FontStyle.medium LanguageStyle
     ]
   , linearLayout
     [ width MATCH_PARENT
     , height $ V 1
     , visibility if (isLastItem) then GONE else VISIBLE
     , background config.grey900
     ][]
  ]
chatComponent :: forall w. Config -> ChatComponent -> Boolean -> String -> PrestoDOM (Effect Unit) w
chatComponent state config isLastItem userType = 
  PrestoAnim.animationSet [ if state.userConfig.appType == config.sentBy then (translateInXForwardAnim $ if isLastItem then true else false) else (translateInXBackwardAnim $ if isLastItem then true else false) ] $
  linearLayout
  [height WRAP_CONTENT
  , width MATCH_PARENT
  , margin (getChatConfig state config.sentBy isLastItem).margin
  , gravity (getChatConfig state config.sentBy isLastItem).gravity
  , orientation VERTICAL
  , onAnimationEnd (\action ->
      if isLastItem then do
        _ <- scrollToBottom (getNewIDWithTag "ChatScrollView")
        pure unit
      else
        pure unit) (const NoAction)
  ][ linearLayout
     [ padding (Padding 12 12 12 12)
     , margin (MarginBottom 4)
     , height WRAP_CONTENT
     , width $ if (os == "IOS" && (STR.length config.message) > (if state.languageKey == "HI_IN" then 50 else 30) ) then MATCH_PARENT else WRAP_CONTENT
     , background (getChatConfig state config.sentBy isLastItem).background
     , cornerRadii (getChatConfig state config.sentBy isLastItem).cornerRadii
     , gravity (getChatConfig state config.sentBy isLastItem).gravity
     ][ textView
        [ text (config.message)
        , textSize FontSize.a_14
        , singleLine false
        , lineHeight "18"
        , color (getChatConfig state config.sentBy isLastItem).textColor
        , fontStyle $ FontStyle.medium LanguageStyle
        ]
      ]
    , textView
      [ text config.timeStamp
      , textSize FontSize.a_10
      , color state.black800
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
  ]
  
getConfig :: String -> {margin :: Int, customerVisibility :: Visibility, driverVisibility :: Visibility}
getConfig appType = 
  if appType == "Customer" then
    {
      margin : 60,
      customerVisibility : VISIBLE,
      driverVisibility : GONE 
    }
  else 
    {
      margin : 30,
      customerVisibility : GONE,
      driverVisibility : VISIBLE 
    }

getChatConfig :: Config -> String -> Boolean -> {margin :: Margin, gravity :: Gravity, background :: String, cornerRadii :: Corners, textColor :: String}
getChatConfig state sentBy isLastItem = 
  if state.userConfig.appType == sentBy then 
    { 
      margin : (Margin ((screenWidth unit)/4) 24 0 (if os == "IOS" && isLastItem then 12 else 0)),
      gravity : RIGHT ,
      background : state.blue800,
      cornerRadii : (Corners 16.0 true true false true),
      textColor :  state.white900
    }
  else 
    { margin : (Margin 0 24 ((screenWidth unit)/4) (if os == "IOS" && isLastItem then 12 else 0)),
      gravity :  LEFT,
      background : state.grey900,
      cornerRadii : (Corners 16.0 true true true false ),
      textColor :   state.black800
    }