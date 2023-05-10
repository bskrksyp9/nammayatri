{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverProfileScreen.ComponentConfig where

import Components.PopUpModal as PopUpModal
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import Screens.Types as ST
import EN

logoutPopUp :: ST.DriverProfileScreenState -> PopUpModal.Config
logoutPopUp  state = let 
  config' = PopUpModal.config
  popUpConfig' = config' {
    primaryText {text = (getString LOGOUT)},
    secondaryText {text = (getString ARE_YOU_SURE_YOU_WANT_TO_LOGOUT)},
    option1 {text = (getString GO_BACK), testIdText = (getEN GO_BACK)},
    option2 {text = (getString LOGOUT), testIdText = (getEN LOGOUT)}
  }
  in popUpConfig'