{-# LANGUAGE TypeApplications #-}

module API.Status where

import API.Confirm.Coerce
import API.Utils
import Beckn.Mock.App
import Beckn.Mock.Exceptions
import Beckn.Mock.Utils
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Logging
import qualified Control.Monad.Catch as C
import Core.OnStatus
import Core.Status
import Environment
import ExternalAPI
import qualified Redis
import Relude

statusServer :: BecknReq StatusMessage -> MockM AppEnv AckResponse
statusServer statusReq@(BecknReq ctx msg) = do
  mockLog INFO $ "got confirm request: " <> show statusReq
  context' <- buildOnActionContext ON_STATUS ctx
  let orderId = msg.order.id
  mockLog INFO $ "reading order with orderId=" <> orderId
  eithCtxOrd <- C.try @(MockM AppEnv) @MockException (Redis.readOrder orderId)

  _ <- mockFork $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    let eithOnStatusMsg = bimap (textToError . show) (OnStatusMessage . coerceOrderStatus . snd) eithCtxOrd
        onStatusReq = BecknCallbackReq context' eithOnStatusMsg
    callBapOnStatus onStatusReq
  pure Ack
