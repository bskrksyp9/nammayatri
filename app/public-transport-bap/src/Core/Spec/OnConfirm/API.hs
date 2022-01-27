module Core.Spec.OnConfirm.API where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import Core.Spec.OnConfirm
import Servant

type API =
  "on_confirm"
    :> ReqBody '[JSON] (BecknCallbackReq OnConfirmMessage)
    :> Post '[JSON] AckResponse
