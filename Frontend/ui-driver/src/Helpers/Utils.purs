{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.Utils
    ( module Helpers.Utils
    , module ReExport
    ) where

-- import Prelude (Unit, bind, discard, identity, pure, show, unit, void, ($), (<#>), (<$>), (<*>), (<<<), (<>), (>>=))
import Effect (Effect)
import Prelude (Unit, bind, pure, discard, unit, void, ($), identity, (<*>), (<#>), (+), (<>))
import Data.Traversable (traverse)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Juspay.OTP.Reader (initiateSMSRetriever)
import Juspay.OTP.Reader.Flow as Reader
import Data.Maybe (Maybe(..), fromMaybe)
import Juspay.OTP.Reader as Readers
import Data.Array.NonEmpty (fromArray)
import Effect.Class (liftEffect)
import Screens.Types (AllocationData, YoutubeData, LeaderBoardDay, LeaderBoardWeek)
import Language.Strings (getString)
import Language.Types(STR(..))
import Prelude ((/),(*),(-))
import Data.Array ((!!)) as DA
import Data.String (Pattern(..), split) as DS
import Data.Number (pi, sin, cos, asin, sqrt)
-- import Control.Monad.Except (runExcept)
-- import Data.Array.NonEmpty (fromArray)
-- import Data.DateTime (Date, DateTime)
-- import Data.Either (Either(..))
-- import Data.JSDate (parse, toDateTime)
-- import Data.Maybe (Maybe(..))
-- import Data.Newtype (unwrap, class Newtype)
-- import Data.Traversable (traverse)
-- import Effect (Effect)
-- import Effect.Class (liftEffect)
-- import Foreign.Generic (class Decode, class Encode,decodeJSON, encodeJSON)
-- import Effect.Aff (error, killFiber, launchAff, launchAff_)
-- import Effect.Console (logShow)
-- import Effect.Timer (setTimeout, TimeoutId)
-- import Engineering.Helpers.Commons (flowRunner, liftFlow)
-- import Juspay.OTP.Reader (initiateSMSRetriever)
-- import Juspay.OTP.Reader.Flow as Reader
-- import Juspay.OTP.Reader as Readers
-- import Presto.Core.Flow (Flow)
-- import Types.App (GlobalState)
-- foreign import getTimer :: forall action. String -> String ->String -> (action -> Effect Unit)  -> (String -> action)  -> Effect Unit
-- foreign import get15sTimer :: forall action. (action -> Effect Unit) -> (String -> action)  -> Effect Unit
-- foreign import get5sTimer :: forall action. (action -> Effect Unit) -> (String -> action)  -> Effect Unit
-- foreign import get10sTimer :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
-- -- foreign import getCurrentLatLongImpl  :: Effect String
import Engineering.Helpers.Commons (parseFloat, setText', getCurrentUTC) as ReExport


foreign import shuffle :: forall a. Array a -> Array a
foreign import generateUniqueId :: Unit -> String
foreign import storeCallBackTime :: forall action. (action -> Effect Unit) -> (String -> String -> String -> action)  -> Effect Unit
foreign import getTime :: Unit -> Int
foreign import countDown :: forall action. Int -> String -> (action -> Effect Unit) -> (Int -> String -> String -> String-> action)  -> Effect Unit
foreign import hideSplash :: Effect Unit
foreign import startTimer :: forall action. Int -> (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import convertKmToM :: String -> String
foreign import convertUTCtoISC :: String -> String -> String
foreign import differenceBetweenTwoUTC :: String -> String -> Int
foreign import clearTimer :: String -> Unit
foreign import clearPopUpTimer :: String -> Unit
foreign import clearAllTimer :: String -> Unit
foreign import toString :: forall a. a-> String
foreign import toInt :: forall a. a -> String
foreign import setRefreshing :: String -> Boolean -> Unit
foreign import setEnabled :: String -> Boolean -> Unit
foreign import decodeErrorCode :: String -> String
foreign import decodeErrorMessage :: String -> String
foreign import storeCallBackForNotification :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import secondsLeft :: String -> Int
foreign import objectToAllocationType :: String -> AllocationData
foreign import getcurrentdate :: String -> String
foreign import launchAppSettings :: Unit -> Effect Unit
foreign import setYoutubePlayer :: YoutubeData -> String -> String -> Unit
foreign import getTimeStampString :: String -> String
foreign import addMediaPlayer :: String -> String -> Effect Unit
foreign import removeMediaPlayer :: String -> Effect Unit
foreign import getVideoID :: String -> String
foreign import getImageUrl :: String -> String

-- -- ####### MAP FFI ######## -----
foreign import currentPosition  :: String -> Effect Unit

otpRule :: Reader.OtpRule
otpRule = Reader.OtpRule {
  matches : {
    sender : [],
    message : "is your OTP for login to Namma Yatri App"
  },
  otp : "\\d{4}",
  group : Nothing
}

startOtpReciever :: forall action. (String -> action) -> (action -> Effect Unit) -> Effect (Effect Unit)
startOtpReciever action push = do
  fiber <- launchAff $ do
    otpListener <- traverse Readers.getOtpListener $ fromArray [ Readers.smsRetriever ]
    _ <- traverse identity $ (otpListener <#> _.setOtpRules) <*> Just [otpRule]
    message <- traverse identity $ (otpListener <#> _.getNextOtp)
    case message of
      Just (Readers.Otp val _ _) -> liftEffect $ push $ action val
      _ -> pure unit
    void $ initiateSMSRetriever
    liftEffect $ startOtpReciever action push
  pure $ launchAff_ $ killFiber (error "Failed to Cancel") fiber


getCorrespondingErrorMessage :: String -> String
getCorrespondingErrorMessage errorCode = case errorCode of
  "IMAGE_VALIDATION_FAILED" -> (getString IMAGE_VALIDATION_FAILED)
  "IMAGE_NOT_READABLE" -> (getString IMAGE_NOT_READABLE)
  "IMAGE_LOW_QUALITY" -> (getString IMAGE_LOW_QUALITY)
  "IMAGE_INVALID_TYPE" -> (getString IMAGE_INVALID_TYPE)
  "IMAGE_DOCUMENT_NUMBER_MISMATCH" -> (getString IMAGE_DOCUMENT_NUMBER_MISMATCH)
  "IMAGE_EXTRACTION_FAILED" -> (getString IMAGE_EXTRACTION_FAILED)
  "IMAGE_NOT_FOUND" -> (getString IMAGE_NOT_FOUND)
  "IMAGE_NOT_VALID" -> (getString IMAGE_NOT_VALID)
  "DRIVER_ALREADY_LINKED" -> (getString DRIVER_ALREADY_LINKED)
  "DL_ALREADY_UPDATED" -> (getString DL_ALREADY_UPDATED)
  "DL_ALREADY_LINKED"  -> (getString DL_ALREADY_LINKED)
  "RC_ALREADY_LINKED" -> (getString RC_ALREADY_LINKED)
  "RC_ALREADY_UPDATED" -> (getString RC_ALREADY_UPDATED)
  "UNPROCESSABLE_ENTITY" -> (getString PLEASE_CHECK_FOR_IMAGE_IF_VALID_DOCUMENT_IMAGE_OR_NOT)
  _                      -> (getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER)

-- -- type Locations = {
-- --     paths :: Array Paths
-- -- }


-- -- type Paths = {
-- --     points :: Points
-- -- }

-- -- type Points = {
-- --     type :: String
-- -- ,   coordinates :: Array Point
-- -- }

-- -- type Point = Array Number

-- -- type Markers = {
-- --     markerObject :: Array MarkerObject
-- -- }

-- -- type MarkerObject = {
-- --     type :: String,
-- --     title :: String,
-- --     coordinates :: Array Number
-- -- }

-- -- newtype LocationLatLong = LocationLatLong
-- --   { lat :: String
-- --   , long :: String
-- --   }

-- -- derive instance genericLocationLatLong :: Generic LocationLatLong _
-- -- derive instance newtypeLocationLatLong :: Newtype LocationLatLong _
-- -- instance encodeLocationLatLong :: Encode LocationLatLong where encode = defaultEncode
-- -- instance decodeLocationLatLong :: Decode LocationLatLong where decode = defaultDecode

getDistanceBwCordinates :: Number -> Number -> Number -> Number -> Number
getDistanceBwCordinates lat1 long1 lat2 long2 = do
    let latPoint1 = toRad (lat1)
    let lngPoint1 = toRad (long1)
    let latPoint2 = toRad (lat2)
    let lngPoint2 = toRad (long2)
    let dlong = toRad (long2 - (long1))
    let lati1 = toRad (lat1)
    let lati2 = toRad (lat2)
    let dist = sin ((latPoint2 - latPoint1) / 2.0 ) * sin ((latPoint2 - latPoint1) / 2.0 ) + cos(latPoint1) * cos(latPoint2) * sin ((lngPoint2 - lngPoint1) / 2.0 ) * sin ((lngPoint2 - lngPoint1) / 2.0 )
    let dist1 = (2.0 * 6371.0 * asin ( sqrt dist))
    dist1

toRad :: Number -> Number
toRad n = (n * pi) / 180.0
