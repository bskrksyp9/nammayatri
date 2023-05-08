{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.RideBooking.Confirm where

import qualified API.UI.Confirm as UC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Quote as Quote
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Servant

data RideConfirmEndPoint = ConfirmEndPoint
  deriving (Show, Read)

derivePersistField "RideConfirmEndPoint"

type API =
  "confirm"
    :> CustomerConfirmAPI

type CustomerConfirmAPI =
  "rideSearch"
    :> Capture "customerId" (Id DP.Person)
    :> "quotes"
    :> Capture "quoteId" (Id Quote.Quote)
    :> "confirm"
    :> Post '[JSON] UC.ConfirmRes

handler :: FlowServer API
handler =
  callConfirm

callConfirm :: Id DP.Person -> Id Quote.Quote -> FlowHandler UC.ConfirmRes
callConfirm = UC.confirm
