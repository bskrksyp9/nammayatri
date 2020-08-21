{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Status where

import App.Types
import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Status
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (parseBaseUrl)

status :: Organization -> StatusReq -> FlowHandler AckResponse
status org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  let mAppUrl = parseBaseUrl . toString =<< req ^. #context . #_ac_id
  let context =
        (req ^. #context)
          { _ac_id = bppNwAddr
          }
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad ac_id"
    Just appUrl ->
      fork "Status" $ do
        statusMessage <- mkStatusMessage
        AckResponse {} <-
          callClient "status" appUrl $
            client
              onStatusAPI
              cbApiKey
              CallbackReq
                { context = context {_action = "on_status"},
                  contents = Right statusMessage
                }
        pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkStatusMessage :: Flow StatusResMessage
mkStatusMessage = do
  L.runIO $ threadDelay 0.5e6
  return $ StatusResMessage example
