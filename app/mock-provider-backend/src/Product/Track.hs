{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Track where

import Beckn.Types.API.Callback
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Track
import Beckn.Utils.Common
import Control.Monad.Reader (withReaderT)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant.Client (parseBaseUrl)
import System.Environment (lookupEnv)

track :: () -> TrackReq -> FlowHandlerR r AckResponse
track _unit req = withReaderT (\(EnvR rt e) -> EnvR rt (EnvR rt e)) . withFlowHandler $ do
  bppNwAddr <- L.runIO $ lookupEnv "MOCK_PROVIDER_NW_ADDRESS"
  let mAppUrl = parseBaseUrl . toString =<< req ^. #context . #_ac_id
      context =
        (req ^. #context)
          { _ac_id = fromString <$> bppNwAddr -- update caller id
          }
  case mAppUrl of
    Nothing -> L.logError @Text "mock_provider_backend" "Bad ac_id"
    Just appUrl ->
      forkAsync "Track" $ do
        trackMessage <- mkTrackMessage
        AckResponse {} <-
          callClient "track" appUrl $
            client
              onTrackAPI
              "test-provider-2-key"
              CallbackReq
                { context = context {_action = "on_track"},
                  contents = Right trackMessage
                }
        pass
  return
    AckResponse
      { _context = context,
        _message = ack "ACK",
        _error = Nothing
      }

mkTrackMessage :: FlowR r TrackResMessage
mkTrackMessage = do
  L.runIO $ threadDelay 0.5e6
  return $ TrackResMessage example
