{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Update where

import App.Types
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import Beckn.Types.FMD.API.Update
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Logging (Log (..))
import Data.Aeson (encode)
import EulerHS.Prelude

updateCb :: Organization -> OnUpdateReq -> FlowHandler AckResponse
updateCb _org req = withFlowHandler $ do
  let resp = AckResponse (req ^. #context) (ack "ACK") Nothing
  logDebug "mock_app_backend" $ "update_cb: req: " <> decodeUtf8 (encode req) <> ", resp: " <> show resp
  case req ^. #context . #_bpp_uri of
    Nothing -> logError "mock-app-backend" "Bad bpp_nw_address"
    Just _ -> logDebug "mock-app-backend" "Update delivered Successfully"
  return resp
