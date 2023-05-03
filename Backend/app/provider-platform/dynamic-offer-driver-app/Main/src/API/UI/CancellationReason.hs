{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.CancellationReason
  ( CancellationReasonListRes,
    API,
    handler,
  )
where

import qualified Beckn.Types.Core.Taxi.API.CancellationReasons as CR
import Beckn.Types.Core.Taxi.CancellationReasons.Types (CancellationReasonsReq)
import qualified Beckn.Types.Core.Taxi.CancellationReasons.Types as SCR
import qualified Domain.Action.UI.CancellationReason as DCancellationReason
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  CR.CancellationReasonsAPI
    :<|> "cancellationReason"
      :> ( "list"
             :> TokenAuth
             :> Get '[JSON] CancellationReasonListRes
         )

handler :: FlowServer API
handler = getCancellationReasons :<|> list

type CancellationReasonListRes = [SCR.CancellationReasonAPIEntity]

list :: Id Person.Person -> FlowHandler CancellationReasonListRes
list _ = withFlowHandlerAPI DCancellationReason.list

getCancellationReasons :: CancellationReasonsReq -> FlowHandler SCR.CancellationReasons
getCancellationReasons _ = withFlowHandlerAPI DCancellationReason.getCancellationReasons
