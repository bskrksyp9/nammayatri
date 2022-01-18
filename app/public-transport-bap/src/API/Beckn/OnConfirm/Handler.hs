module API.Beckn.OnConfirm.Handler where

import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult)
import Core.ACL.OnConfirm
import Core.Spec.API.OnConfirm
import qualified Core.Spec.Common.Context as Context
import Domain.Endpoints.Beckn.OnConfirm
import Tools.Context

handler :: SignatureAuthResult -> FlowServer OnConfirmAPI
handler _ onConfirmCb = withFlowHandlerAPI . withTransactionIdLogTag onConfirmCb $ do
  validateContext Context.ON_CONFIRM $ onConfirmCb.context
  case onConfirmCb.contents of
    Right msg -> handleOnConfirm $ mkDomainOnConfirm (Id onConfirmCb.context.transaction_id) msg
    Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
  pure Ack