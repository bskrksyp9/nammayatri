module API.UI.Support
  ( API,
    handler,
    DSupport.SendIssueReq (..),
    DSupport.SendIssueRes,
  )
where

import qualified App.Types as App
import Beckn.Types.Id
import qualified Domain.Action.UI.Support as DSupport
import Domain.Types.Person as Person
import EulerHS.Prelude hiding (length)
import Servant
import Utils.Auth
import Utils.Common

-------- Support Flow----------
type API =
  "support"
    :> ( "sendIssue"
           :> TokenAuth
           :> ReqBody '[JSON] DSupport.SendIssueReq
           :> Post '[JSON] DSupport.SendIssueRes
       )

handler :: App.FlowServer API
handler = sendIssue

sendIssue :: Id Person.Person -> DSupport.SendIssueReq -> App.FlowHandler DSupport.SendIssueRes
sendIssue personId = withFlowHandlerAPI . withPersonIdLogTag personId . DSupport.sendIssue personId