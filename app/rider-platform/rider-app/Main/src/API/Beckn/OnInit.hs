module API.Beckn.OnInit (API, handler) where

import qualified Beckn.ACL.Cancel as CancelACL
import qualified Beckn.ACL.Confirm as ACL
import qualified Beckn.ACL.OnInit as TaxiACL
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Action.UI.Cancel as DCancel
import Domain.Types.CancellationReason
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.Queries.Booking as QRideB

type API = OnInit.OnInitAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onInit

onInit ::
  SignatureAuthResult ->
  OnInit.OnInitReq ->
  FlowHandler AckResponse
onInit (SignatureAuthResult _ _ registryUrl) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnInitReq <- TaxiACL.buildOnInitReq req
  whenJust mbDOnInitReq $ \onInitReq ->
    Redis.whenWithLockRedis (onInitLockKey onInitReq.bppBookingId.getId) 60 $ do
      onInitRes <- DOnInit.onInit registryUrl onInitReq
      booking <- QRideB.findById onInitRes.bookingId >>= fromMaybeM (BookingDoesNotExist onInitRes.bookingId.getId)
      handle (errHandler booking) $
        void $ withShortRetry $ CallBPP.confirm onInitRes.bppUrl =<< ACL.buildConfirmReq onInitRes
  pure Ack
  where
    errHandler booking exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = do
        dCancelRes <- DCancel.cancel booking.id booking.riderId cancelReq
        void . withShortRetry $ CallBPP.cancel dCancelRes.bppUrl =<< CancelACL.buildCancelReq dCancelRes
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = do
        dCancelRes <- DCancel.cancel booking.id booking.riderId cancelReq
        void . withShortRetry $ CallBPP.cancel dCancelRes.bppUrl =<< CancelACL.buildCancelReq dCancelRes
      | otherwise = throwM exc

    cancelReq =
      DCancel.CancelReq
        { reasonCode = CancellationReasonCode "External/Beckn API failure",
          reasonStage = OnConfirm,
          additionalInfo = Nothing
        }

onInitLockKey :: Text -> Text
onInitLockKey id = "Customer:OnInit:BppBookingId-" <> id