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

module API.Dashboard.RideBooking.Profile where

import qualified API.UI.Profile as AP
import qualified Domain.Action.UI.Profile as DProfile
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Servant

data ProfileEndPoint = UpdatePersonEndPoint
  deriving (Show, Read)

derivePersistField "ProfileEndPoint"

type API =
  "profile"
    :> ( CustomerGetProfileAPI
           :<|> CustomerUpdateProfileAPI
       )

type CustomerGetProfileAPI =
  "detail"
    :> Capture "customerId" (Id DP.Person)
    :> Get '[JSON] DProfile.ProfileRes

type CustomerUpdateProfileAPI =
  "update"
    :> Capture "customerId" (Id DP.Person)
    :> ReqBody '[JSON] DProfile.UpdateProfileReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler =
  callGetPersonDetails
    :<|> callUpdatePerson

callGetPersonDetails :: Id DP.Person -> FlowHandler DProfile.ProfileRes
callGetPersonDetails = AP.getPersonDetails

callUpdatePerson :: Id DP.Person -> DProfile.UpdateProfileReq -> FlowHandler APISuccess
callUpdatePerson = AP.updatePerson
