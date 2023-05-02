{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReferralScreen.View where


import Animation (screenAnimationFadeInOut)
import Components.BottomNavBar as BottomNavBar
import Components.BottomNavBar.Controller (navData)
import Components.GenericHeader as GenericHeader
import Components.PrimaryEditText.Views as PrimaryEditText
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Engineering.Helpers.Commons (safeMarginTop, safeMarginBottom, os ,getNewIDWithTag , flowRunner, screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge (openUrlInApp , startTimerWithTime , toast)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (==), (<>) , map , discard , show ,(>), void, (/=), (/), (*), (+), not, (||), negate, (<=))
import PrestoDOM (Gravity(..), Length(..), LetterSpacing(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, lineHeight, linearLayout, margin, onBackPressed, orientation, padding, text, textSize, textView, weight, width, imageView, imageUrl, cornerRadius, onClick, afterRender, visibility,stroke , alpha, relativeLayout , scrollView , alignParentRight, alignParentBottom, imageWithFallback, frameLayout, horizontalScrollView, scrollBarX, scrollBarY, id, gradient, rotation, rotationY)
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Properties (cornerRadii)
import Screens.ReferralScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.ReferralScreen.ScreenData as ReferralScreenData
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Common.Types.App
import Components.PopUpModal as PopUpModal
import Data.Maybe (Maybe(..) ,fromMaybe, fromJust, isJust)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Control.Monad.Except.Trans (runExceptT , lift)
import Control.Transformers.Back.Trans (runBackT)
import Presto.Core.Types.Language.Flow (doAff)
import Helpers.Utils (countDown)
import Screens.ReferralScreen.ComponentConfig
import Data.Array (mapWithIndex, (!!), length, null, index, drop)
import Data.Int (toNumber, ceil)
import Styles.Types as Font
import PrestoDOM.Animation as PrestoAnim

screen :: ST.ReferralScreenState -> Screen Action ST.ReferralScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "ReferralScreen"
  , globalEvents : []
  , eval
  }


view :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimationFadeInOut $
    relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ](
    [linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push (const BackPressed)
    , background Color.white900
    , gravity CENTER
    , afterRender push (const AfterRender)
    ][  linearLayout
        [ width MATCH_PARENT
        , orientation VERTICAL
        , weight 1.0
        , height WRAP_CONTENT
        ](
        [
          GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state),
          linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.greySmoke
            ][]
        ] <> if state.props.stage == ST.SuccessScreen then [commonView push "ny_ic_green_tick" (getString  YOUR_REFERRAL_CODE_IS_LINKED) (getString YOU_CAN_NOW_EARN_REWARDS) state] else []
          <> if state.props.stage == ST.ComingSoonScreen then [commonView push "ny_ic_comming_soon_poster" (getString COMING_SOON) (getString COMING_SOON_DESCRIPTION) state] else []
          <> if state.props.stage == ST.ReferralFlow then  [referralEnrolmentFlow push state, continueButtonView push state] else []
          <> if state.props.stage == ST.QRScreen then [qrScreen push state] else []
          <> if state.props.stage == ST.LeaderBoard then [leaderBoard push state] else [])
        , bottomNavBarView push state
        ]
        , passwordPopUpView push state
        , customerSupportPopUpView state push
    ] <> if state.props.passwordPopUpVisible then [passwordPopUpView push state] else [])

leaderBoard :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
leaderBoard push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , margin (Margin 16 12 16 0)
      , padding (Padding 2 2 2 2)
      , background Color.grey800
      , cornerRadius 30.0
      , gravity CENTER
      ][ leaderBoardTab "Daily LeaderBoard" ST.Daily push state
       , leaderBoardTab "Weekly LeaderBoard" ST.Weekly push state
       ]
    , dateAndTime push state
    , relativeLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , weight 1.0
      ]([ leaderBoardRanks state
        , dateSelector push state
        ] <> if state.props.currentDriverData.rank > 10 then [currentDriverRank state] else []
       )
   ]

currentDriverRank :: forall w . ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
currentDriverRank state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity BOTTOM
  , alignParentBottom "true,-1"
  , cornerRadii $ Corners 18.0 true true false false
  ][ rankCard state.props.currentDriverData true state
   ]

dateAndTime :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
dateAndTime push state =
  let date = state.props.selectedDay
      week = state.props.selectedWeek
  in
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , margin (Margin 16 0 16 0)
    , padding (Padding 8 15 8 15)
    , gravity CENTER_VERTICAL
    ][  linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , weight 1.0
        , onClick push (const $ DateSelectorAction)
        ][  textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $  if state.props.leaderBoardType == ST.Daily then
                        (show date.date) <> " " <> date.month <> ", " <> (show date.year)
                      else
                        (show week.startDate) <> " " <> week.startMonth <> " - " <> (show week.endDate) <> " " <> week.endMonth
            , textSize FontSize.a_16
            , color Color.black800
            ]
          , imageView
            [ width (V 24)
            , height (V 24)
            , margin (MarginLeft 12)
            , imageWithFallback if state.props.showDateSelector then
                                  "ny_ic_chevron_up_blue,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_up_blue.png"
                                else
                                  "ny_ic_calendar_blue,https://assets.juspay.in/nammayatri/images/driver/ny_ic_calendar_blue.png"
            ]
        ]
      , textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text "Last Updated: 5:23 PM" -- TODO :: when to update time ?
        , textSize FontSize.a_12
        , color Color.black700
        , gravity RIGHT
        , weight 1.0
        ]
    ]

leaderBoardRanks :: forall w . ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
leaderBoardRanks state =
  let currentDriverRank = state.props.currentDriverData.rank
  in
    scrollView
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    ][ linearLayout
       [ width MATCH_PARENT
       , height MATCH_PARENT
       , orientation VERTICAL
       , padding (PaddingBottom if currentDriverRank > 10 then 56 else 0)
       ] $ (if currentDriverRank <= 10 then [congratsBar currentDriverRank] else [])
           <> [ topRankers state.props.rankersData
              , otherRankers state
              ]
     ]

congratsBar :: forall w . Int -> PrestoDOM (Effect Unit) w
congratsBar rank =
  linearLayout
  ([ width MATCH_PARENT
   , height (V 44)
   , margin (Margin 16 15 16 0)
   , cornerRadius 8.0
   ] <> if rank == 1 then [gradient (Linear 270.0 ["#FEE8B8","#D3A119","#C59311", "#E1BC5B"])] else [background Color.blue800]
  )[  frameLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      ]([  linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , gravity CENTER
          ][  textView
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text  if rank == 1 then
                        "Congratulations! You are RANK 1 🏆"
                      else
                        "Congratulations! You are RANK " <> (show rank) <> " 🎉"
              , color Color.white900
              , textSize FontSize.a_18
              ]
           ]
       ] <> if rank == 1 then [shineAnimation rank] else [])
   ]

shineAnimation :: forall w . Int -> PrestoDOM (Effect Unit) w
shineAnimation rank =
  PrestoAnim.animationSet
  [ PrestoAnim.Animation
    [ PrestoAnim.duration 2500
    , PrestoAnim.fromX $ (- 200)
    , PrestoAnim.toX $ (screenWidth unit) + 200
    , PrestoAnim.repeatCount PrestoAnim.Infinite
    ] true
  ] $ linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][ linearLayout
        [ width (V 10)
        , height (V 200)
        , margin (MarginRight 20)
        ][]
      , linearLayout
        [ width (V 10)
        , height (V 100)
        , background "#30FFFFFF"
        , rotation 20.0
        , margin (MarginRight 10)
        ][]
      , linearLayout
        [ width (V 25)
        , height (V 100)
        , background "#30FFFFFF"
        , rotation 20.0
        ][]
      ]

otherRankers :: forall w . ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
otherRankers state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ] (mapWithIndex (\index item ->
                      rankCard item false state
                  ) (drop 3 state.props.rankersData)
    )

rankCard :: forall w . ST.RankCardData -> Boolean -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
rankCard item aboveThreshold state =
  let currentDriverData = state.props.currentDriverData
  in
    linearLayout
    ([ width MATCH_PARENT
    , height WRAP_CONTENT
    , stroke $ (if aboveThreshold then "0," else "1,") <> Color.grey900
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding if aboveThreshold then (Padding 24 8 24 8) else (Padding 8 8 8 8)
    , background if aboveThreshold || item == currentDriverData then Color.blue800 else Color.white900
    ] <> (if not aboveThreshold then [margin (Margin 16 8 16 8)] else [])
      <> (if aboveThreshold then [cornerRadii (Corners 12.0 true true false false)] else [cornerRadius 12.0])
     )[ linearLayout
       [ width WRAP_CONTENT
       , height WRAP_CONTENT
       , gravity CENTER_VERTICAL
       ][ linearLayout
          [ width (V 45)
          , height WRAP_CONTENT
          , gravity CENTER_HORIZONTAL
          , margin (MarginHorizontal 8 8)
          ][ textView
             [ width WRAP_CONTENT
             , height WRAP_CONTENT
             , text $ show item.rank
             , textSize FontSize.a_14
             , color if aboveThreshold || item == currentDriverData then Color.white900 else Color.black800
            --  , margin (MarginHorizontal 8 8)
             ]
           ]
        , imageView
          [ width (V 30)
          , height (V 30)
          , cornerRadius 30.0
          , margin (MarginHorizontal 8 10)
          , imageWithFallback "ny_ic_general_profile,https://assets.juspay.in/nammayatri/images/driver/ny_ic_general_profile.png"
          ]
        , textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ item.goodName <> if aboveThreshold then " (you)" else ""
          , textSize FontSize.a_14
          , color if aboveThreshold || item == currentDriverData then Color.white900 else Color.black800
          ]
        ]
      , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity RIGHT
        , weight 1.0
        ][ textView
           [ width WRAP_CONTENT
           , height WRAP_CONTENT
           , gravity CENTER_VERTICAL
           , text $ (show item.rides) <> " Rides"
           , textSize FontSize.a_16
           , color if aboveThreshold || item == currentDriverData then Color.white900 else Color.black800
           ]
         ]
     ]

topRankers :: forall w . Array ST.RankCardData -> PrestoDOM (Effect Unit) w
topRankers rankersList =
  let rank1 = fromMaybe ReferralScreenData.dummyRankData (index rankersList 0)
      rank2 = fromMaybe ReferralScreenData.dummyRankData (index rankersList 1)
      rank3 = fromMaybe ReferralScreenData.dummyRankData (index rankersList 2)
  in
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , margin (Margin 16 15 16 15)
    , gradient (Linear 0.0 ["#F5F8FF", "#E2EAFF"])
    , orientation HORIZONTAL
    , gravity CENTER
    , padding (PaddingVertical 20 24)
    , cornerRadius 16.0
    ][ rankers 72 2 Color.green900 false FontSize.a_16 rank2 "ny_ic_rank2,https://assets.juspay.in/nammayatri/images/driver/ny_ic_rank2.png"
     , rankers 102 1 Color.yellow900 true FontSize.a_22 rank1 "ny_ic_rank1,https://assets.juspay.in/nammayatri/images/driver/ny_ic_rank1.png"
     , rankers 72 3 Color.orange900 false FontSize.a_16 rank3 "ny_ic_rank3,https://assets.juspay.in/nammayatri/images/driver/ny_ic_rank3.png"
     ]

rankers :: forall w . Int -> Int -> String -> Boolean -> Font.FontSize -> ST.RankCardData -> String -> PrestoDOM (Effect Unit) w
rankers size rank themeColor showCrown fontSize detail imageUrl =
  let bottomMargin = ceil ( (toNumber size) / 7.0 )
      rankWidth = bottomMargin * 2 + 5
  in
    linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , weight 1.0
    , gravity CENTER
    , orientation VERTICAL
    , margin (MarginHorizontal 10 10)
    ][ frameLayout
       [ width (V $ size)
       , height (V $ size + bottomMargin + 30)
       ][ linearLayout
          [ width MATCH_PARENT
          , height (V 22)
          , gravity CENTER_HORIZONTAL
          ][ imageView
             [ imageWithFallback "ny_ic_crown,https://assets.juspay.in/nammayatri/images/driver/ny_ic_crown.png"
             , visibility if rank == 1 then VISIBLE else GONE
             ]
           ]
        , linearLayout
          [ width (V $ size)
          , height (V $ size)
          , background Color.white900
          , cornerRadius (toNumber size)
          , stroke ("2," <> themeColor)
          , margin (MarginTop 28)
          ][ imageView
             [ width MATCH_PARENT
             , height MATCH_PARENT
             , imageWithFallback imageUrl
             ]
           ]
        , linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , gravity BOTTOM
          ][ linearLayout
             [ width MATCH_PARENT
             , height (V $ bottomMargin * 2)
             , gravity CENTER
             ][ textView
                [ width (V $ rankWidth)
                , height MATCH_PARENT
                , text $ show rank
                , textSize fontSize
                , fontStyle  $ FontStyle.bold LanguageStyle
                , color if rank ==  1 then Color.black900 else Color.white900
                , background themeColor
                , gravity CENTER
                , cornerRadius 14.0
                ]
              ]
           ]
        ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , orientation VERTICAL
        ][ textView
           [ width MATCH_PARENT
           , height MATCH_PARENT
           , text $ (show detail.rides) <> " Rides"
           , gravity CENTER
           , textSize FontSize.a_18
           , fontStyle  $ FontStyle.bold LanguageStyle
           , color Color.black800
           ]
         , textView
           [ width MATCH_PARENT
           , height MATCH_PARENT
           , text detail.goodName
           , gravity CENTER
           , textSize FontSize.a_12
           , color Color.black700
           ]
         ]
     ]

dateSelector :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
dateSelector push state =
  let leaderBoardType = state.props.leaderBoardType
  in
    horizontalScrollView
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.white900
    , cornerRadii $ Corners 18.0 false false true true
    , stroke $ "1," <> Color.grey900
    , visibility if state.props.showDateSelector then VISIBLE else GONE
    , scrollBarX false
    , id (getNewIDWithTag "DateSelector")
    ][  linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , padding (Padding 12 18 12 20)
        ] if leaderBoardType == ST.Daily then
            (mapWithIndex (\index item ->
                  linearLayout
                  [ width (V 50)
                  , height (V 50)
                  , cornerRadius 50.0
                  , background if item == state.props.selectedDay then Color.blue600 else Color.grey700
                  , margin (MarginHorizontal 4 4)
                  , gravity CENTER
                  , orientation VERTICAL
                  , stroke $ "1," <> if item == state.props.selectedDay then Color.blue800 else Color.grey700
                  , onClick push (const $ ChangeDate (ST.DaySelector item))
                  ][  textView
                      [ text $ show item.date
                      , textSize FontSize.a_16
                      , color if item == state.props.selectedDay then Color.blue800 else Color.black700
                      ]
                    , textView
                      [ text item.month
                      , textSize FontSize.a_10
                      , color if item == state.props.selectedDay then Color.blue800 else Color.black700
                      ]
                   ]
               ) state.props.days
            )
          else
            (mapWithIndex (\index item ->
                  linearLayout
                  [ width (V 140)
                  , height (V 50)
                  , cornerRadius 50.0
                  , background if item == state.props.selectedWeek then Color.blue600 else Color.grey700
                  , margin (MarginHorizontal 4 4)
                  , gravity CENTER
                  , orientation HORIZONTAL
                  , stroke $ "1," <> if item == state.props.selectedWeek then Color.blue800 else Color.grey700
                  , onClick push (const $ ChangeDate (ST.WeekSelector item))
                  ][  textView
                      [ text $ (show item.startDate) <> " " <> item.startMonth <> " - " <> (show item.endDate) <> " " <> item.endMonth
                      , textSize FontSize.a_16
                      , color if item == state.props.selectedWeek then Color.blue800 else Color.black700
                      ]
                   ]
               ) state.props.weeks
            )
     ]

leaderBoardTab :: forall w . String -> ST.LeaderBoardType -> (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
leaderBoardTab _text leaderBoardType push state =
  textView
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text _text
  , background if leaderBoardType == state.props.leaderBoardType then Color.black900 else Color.grey800
  , padding (Padding 8 8 8 8)
  , weight 1.0
  , gravity CENTER
  , color if leaderBoardType == state.props.leaderBoardType then Color.white900 else Color.black700
  , cornerRadius 20.0
  , textSize FontSize.a_14
  , onClick (\action ->
              if state.props.leaderBoardType /= leaderBoardType then do
                _ <- push action
                pure unit
              else pure unit
            ) (const $ ChangeLeaderBoardtab leaderBoardType)
  ]

bottomNavBarView :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
bottomNavBarView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][BottomNavBar.view (push <<< BottomNavBarAction) (navData 2)]


commonView :: forall w . (Action -> Effect Unit) -> String -> String -> String -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
commonView push img title description state=
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , afterRender (\action -> do
                        if state.props.stage == ST.SuccessScreen then do
                          void $ launchAff $ flowRunner $ runExceptT $ runBackT $ lift $ lift $ doAff do
                            if (os == "IOS") then liftEffect $ startTimerWithTime (show state.props.seconds) state.props.id "1" push SuccessScreenExpireCountDwon
                              else liftEffect $ countDown state.props.seconds state.props.id push SuccessScreenExpireCountDwon
                        else pure unit
                        push action
                  ) (const SuccessScreenRenderAction)
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gravity CENTER
        , padding (PaddingHorizontal 16 16)
        , weight 1.0
        ]
        [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , gravity CENTER
          ]
          [ imageView
              [ height $ if img == "ny_ic_comming_soon_poster" then V 290 else V 112
              , width $ if img == "ny_ic_comming_soon_poster" then V 234 else V 112
              , imageUrl img
              , margin $ if img == "ny_ic_comming_soon_poster" then (Margin 0 0 0 0) else (MarginBottom 72)
              , onClick push (const if img == "ny_ic_comming_soon_poster" then EnableReferralFlow else EnableReferralFlowNoAction)
              ]
          , textView
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , gravity CENTER
              , text title
              , color Color.black900
              , textSize $ FontSize.a_18
              , fontStyle $ FontStyle.bold LanguageStyle
              ]
          , textView
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , gravity CENTER
              , text description
              , color Color.black700
              , textSize $ FontSize.a_14
              , fontStyle $ FontStyle.regular LanguageStyle
              , padding (PaddingVertical 10 10)
              ]
          ]
        ]
    ]


referralEnrolmentFlow :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
referralEnrolmentFlow push state =
    scrollView
    [
      width MATCH_PARENT
    , weight 1.0
    , height WRAP_CONTENT
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , orientation VERTICAL
        , height WRAP_CONTENT
        , weight 1.0
        , padding $ Padding 16 16 16 16
        ]
        [ textView (
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , text (getString DRIVER_DETAILS)
          , textSize $ FontSize.a_14
          , color Color.greyTextColor
          , alpha 0.8
          ] <> FontStyle.tags TypoGraphy)
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , background Color.grey700
          , cornerRadius 5.0
          , margin $ MarginTop 8
          , padding $ Padding 16 15 0 16
          ]
          [
            textView (
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text state.data.driverInfo.driverName
            , color Color.black800
            , textSize $ FontSize.a_14
            ] <> FontStyle.body1 TypoGraphy)
          , textView (
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , text $ "+91 "<> (fromMaybe "" state.data.driverInfo.driverMobile)
            , color Color.black800
            , textSize $ FontSize.a_16
            , margin (MarginVertical 8 8)
            ] <> FontStyle.body1 TypoGraphy)
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            , padding $ Padding 3 3 3 3
            , background Color.golden
            , cornerRadius 4.0
            , gravity CENTER
            ]
            [
              linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , orientation VERTICAL
              , padding $ Padding 7 4 7 4
              , background Color.golden
              , cornerRadius 4.0
              , stroke "1,#454545"
              , gravity CENTER
              ]
              [
                textView (
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , text state.data.driverInfo.vehicleRegNumber
                , color Color.black800
                , textSize $ FontSize.a_16
                ] <> FontStyle.subHeading1 TypoGraphy)
              ]
            ]
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (MarginTop 10)
          ][  PrimaryEditText.view(push <<< PrimaryEditTextAction1) ({
              title: (getString REFERRAL_CODE)
              , hint: (getString REFERRAL_CODE_HINT)
              , valueId: ""
              , isinValid: false
              , error: Just (getString INVALID_MOBILE_NUMBER)
              , pattern : Just "[0-9]*,6"
              , text: ""
              , letterSpacing: PX 0.0
              , id: (getNewIDWithTag "EnterReferralCodeEditText")
              , fontSize : FontSize.a_18
              , type : "number"
            })
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin (MarginTop 10)
          ][  PrimaryEditText.view(push <<< PrimaryEditTextAction2) ({
              title: (getString CONFIRM_REFERRAL_CODE)
              , hint: (getString CONFIRM_REFERRAL_CODE_HINT)
              , valueId: ""
              , isinValid: false
              , error: Just (getString INVALID_MOBILE_NUMBER)
              , pattern : Just "[0-9]*,6"
              , text: ""
              , letterSpacing: PX 0.0
              , id: (getNewIDWithTag "EnterConfirmReferralCoderEditText")
              , fontSize : FontSize.a_18
              , type : "number"
            })
          ]
        ]
      ]

continueButtonView :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
continueButtonView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , alignParentBottom "true,-1"
    , padding $ Padding 16 0 16 16
    ][PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonViewConfig state)]

qrScreen :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
qrScreen push state =
  scrollView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      ][ linearLayout
        [ weight 1.0
        , width MATCH_PARENT
        , orientation VERTICAL
        , padding (Padding 20 0 20 16)
        ][  linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , background Color.yellow900
            , cornerRadius 12.0
            , margin $ MarginTop 24
            , padding (PaddingBottom 5)
            ]
            [
              linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                , margin $ MarginTop 20
                ]
                [
                  imageView
                    [ height $ V 49
                    , width $ V 120
                    , imageUrl "ny_namma_yatri"
                    ]
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                , margin $ MarginTop 16
                ]
                [
                  imageView
                    [ height $ V 288
                    , width $ V 288
                    , imageUrl "ny_ic_qr_code"
                    ]
                ]
            , textView
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity CENTER
              , text (getString YOUR_REFERRAL_CODE)
              , color Color.black900
              , textSize $ FontSize.a_16
              , margin $ MarginTop 8
              ]
            , textView
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity CENTER
              , text (fromMaybe "" state.data.driverInfo.referralCode)
              , color Color.black900
              , textSize $ FontSize.a_24
              , fontStyle $ FontStyle.bold LanguageStyle
              ]
            , linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , gravity CENTER
              , visibility GONE
              , padding (Padding 4 12 4 12)
              ][ linearLayout
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , cornerRadius 24.0
                  , background Color.white900
                  , orientation HORIZONTAL
                  ]
                  [ imageView
                    [ height $ V 30
                    , width $ V 30
                    , padding (PaddingLeft 10)
                    , imageUrl "ic_share"
                    ]
                  , textView
                    [ height MATCH_PARENT
                    , width MATCH_PARENT
                    , textSize $ FontSize.a_12
                    , color Color.black900
                    , padding (Padding 0 6 10 0)
                    , text  (" " <> getString SHARE_OPTIONS <> " ")
                    ]
                  ]
              ]

            ]
        ,   linearLayout
            [  height $ V 80
            , width MATCH_PARENT
            , margin $ MarginTop 16
            , cornerRadius 12.0
            , background if state.data.driverPerformance.referrals.totalActivatedCustomers > 0 then Color.greenGrey else Color.black800
            ]
            [  linearLayout
                [ width WRAP_CONTENT
                , height MATCH_PARENT
                , weight 1.0
                , padding $ Padding 20 13 0 0
                , orientation VERTICAL
                ]
                [ textView
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , gravity LEFT
                  , text if state.data.driverPerformance.referrals.totalActivatedCustomers > 0 then (getString FIRST_REFERRAL_SUCCESSFUL) else (getString AWAITING_REFERRAL_RIDE)
                  , color Color.white900
                  , textSize FontSize.a_14
                  , fontStyle $ FontStyle.semiBold LanguageStyle
                  ]
                , textView
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , gravity LEFT
                  , text (getString CHECK_THIS_SPACE_WHEN_YOU_GET_REFERRAL_ALERT)
                  , color Color.white900
                  , visibility if state.data.driverPerformance.referrals.totalActivatedCustomers > 0 then GONE else VISIBLE
                  , textSize $ FontSize.a_12
                  , fontStyle  $ FontStyle.regular LanguageStyle
                  ]
                , contactUsTextView  push state
                ]
            ,   imageView
                [
                  height $ V 80
                ,  width $ V 118
                ,  margin $ MarginRight 5
                , imageUrl if state.data.driverPerformance.referrals.totalActivatedCustomers > 0 then "ny_ic_auto2" else "ny_ic_auto1"
                ]
            ]
        ,   linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin $ MarginTop 16
            , background Color.grey700
            , cornerRadius 12.0
            , padding $ Padding 16 17 16 17
            , orientation VERTICAL
            ]
            [ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              ]
              [
                textView
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , weight 1.0
                , gravity LEFT
                , text (getString REFERRED_CUSTOMERS)
                , color Color.black800
                , textSize $ FontSize.a_14
                ]
              , textView
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , alignParentRight "true,-1"
                , gravity CENTER
                , text (show state.data.driverPerformance.referrals.totalReferredCustomers)
                , color Color.black800
                , textSize FontSize.a_18
                , fontStyle  $ FontStyle.bold LanguageStyle
                ]
              ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                ]
                [
                  textView
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , gravity LEFT
                  , weight 1.0
                  , text (getString ACTIVATED_CUSTOMERS)
                  , color Color.black800
                  , textSize FontSize.a_14
                  ]
                , textView
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , alignParentRight "true,-1"
                  , gravity CENTER
                  , text (show state.data.driverPerformance.referrals.totalActivatedCustomers)
                  , color Color.black800
                  , textSize FontSize.a_18
                  , fontStyle $ FontStyle.bold LanguageStyle
                  ]
                ]
            ]
        ]
        ]

passwordPopUpView :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
passwordPopUpView push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , visibility if state.props.passwordPopUpVisible then VISIBLE else GONE
  , orientation VERTICAL
  ][PopUpModal.view (push <<< PasswordModalAction) (passwordPopUpConfig state )]

customerSupportPopUpView :: forall w. ST.ReferralScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
customerSupportPopUpView state push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility if state.props.callSupportPopUpVisible then VISIBLE else GONE
  ][PopUpModal.view (push <<< ContactSupportAction) (contactSupportConfig state)]



contactUsTextView :: forall w . (Action -> Effect Unit) -> ST.ReferralScreenState -> PrestoDOM (Effect Unit) w
contactUsTextView push state =
 linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginTop 4
  , visibility if state.data.driverPerformance.referrals.totalActivatedCustomers > 0 then VISIBLE else GONE
  , onClick push $ const GoToAlertScreen
  ][ textView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity LEFT
    , text (getString FOR_UPDATES_SEE_ALERTS)
    , color Color.white900
    , textSize $ FontSize.a_12
    , fontStyle $ FontStyle.regular LanguageStyle
    ]
    , linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.white900
    ][]
  ]


emptyView :: forall w . PrestoDOM (Effect Unit) w
emptyView =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  ][]
