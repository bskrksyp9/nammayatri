{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Main where

import Prelude (Unit, bind, pure, show, unit, ($), (<$>), (<<<), (==), void)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons (flowRunner, liftFlow, getWindowVariable)
import Flow as Flow
import Control.Monad.Except.Trans (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import PrestoDOM.Core (processEvent) as PrestoDom
import Log
import Presto.Core.Types.Language.Flow (throwErr)
import Foreign (MultipleErrors, unsafeToForeign)
import Foreign.Generic (decode)
import Common.Types.App (GlobalPayload)
import Effect.Class (liftEffect)
import Control.Monad.Except (runExcept)
import Data.Maybe (fromMaybe, Maybe(..))
import Screens.Types (AllocationData)

main :: Effect Unit
main = do
  void $ launchAff $ flowRunner $ do
    _ <- pure $ printLog "printLog " "in main"
    resp ← runExceptT $ runBackT $ Flow.baseAppFlow
    case resp of
      Right _ -> pure $ printLog "printLog " "Success in main"
      Left error -> do
        _ <- pure $ printLog "printLog error in main" error
        liftFlow $ main

mainAllocationPop :: String -> AllocationData -> Effect Unit
mainAllocationPop payload_type entityPayload = do
  _ <- pure $ printLog "entity_payload" entityPayload
  payload  ::  Either MultipleErrors GlobalPayload  <- runExcept <<< decode <<< fromMaybe (unsafeToForeign {}) <$> (liftEffect $ getWindowVariable "__payload" Just Nothing)
  case payload of
    Right _ -> void $ launchAff $ flowRunner $ do
      if(payload_type == "NEW_RIDE_AVAILABLE") then
        runExceptT $ runBackT $ (Flow.popUpScreenFlow entityPayload)
        else
          runExceptT $ runBackT $ Flow.homeScreenFlow

    Left e -> void $ launchAff $ flowRunner $ do
      _ <- pure $ printLog "payload type mismatch " ""
      throwErr $ show e

onEvent :: String -> Effect Unit
onEvent "onBackPressed" = PrestoDom.processEvent "onBackPressedEvent" unit
onEvent _ = pure unit

onConnectivityEvent :: String -> Effect Unit
onConnectivityEvent triggertype = do
  void $ launchAff $ flowRunner $ do
    resp ← runExceptT $ runBackT $ Flow.noInternetScreenFlow triggertype
    case resp of
      Right _ -> pure $ printLog "Event" "onConnectivityEvent"
      Left error -> throwErr $ show error
