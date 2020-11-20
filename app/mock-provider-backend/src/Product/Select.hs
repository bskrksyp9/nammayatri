{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Select where

import App.Types
import Beckn.Types.Common
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Select
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)

select :: Organization -> SelectReq -> FlowHandler AckResponse
select org req = withFlowHandler $ do
  bppNwAddr <- nwAddress <$> ask
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  let mAppUrl = req ^. #context . #_bap_uri
      context =
        (req ^. #context)
          { _bpp_uri = bppNwAddr
          }
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad bap_nw_address"
    Just appUrl ->
      fork "Select" $ do
        onSelectMessage <- mkQuote
        AckResponse {} <-
          callClient "select" (req ^. #context) appUrl $
            client
              onSelectAPI
              cbApiKey
              CallbackReq
                { context = context {_action = "on_select"},
                  contents = Right onSelectMessage
                }
        pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkQuote :: Flow SelectOrder
mkQuote = do
  L.runIO $ threadDelay 0.5e6
  return $ SelectOrder example
