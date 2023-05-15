{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module API.Beckn.Confirm (API, handler) where

import qualified Beckn.ACL.Confirm as ACL
import qualified Beckn.ACL.OnConfirm as ACL
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Servant
import qualified SharedLogic.CallBAP as BP
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Person as QPerson

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> Confirm.ConfirmAPI

handler :: FlowServer API
handler = confirm

confirm ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Confirm.ConfirmReq ->
  FlowHandler AckResponse
confirm transporterId (SignatureAuthResult _ subscriber) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Confirm API Flow" "Reached"
    dConfirmReq <- ACL.buildConfirmReq req
    Redis.whenWithLockRedis (confirmLockKey dConfirmReq.bookingId.getId) 60 $ do
      let context = req.context
      dConfirmRes <- DConfirm.handler subscriber transporterId dConfirmReq
      transporter <- QM.findById transporterId >>= fromMaybeM (MerchantNotFound transporterId.getId)
      case dConfirmRes.booking.bookingType of
        DBooking.NormalBooking -> do
          ride <- dConfirmRes.ride & fromMaybeM (RideNotFound dConfirmRes.booking.id.getId)
          now <- getCurrentTime
          driverQuote <- runInReplica $ QDQ.findById (Id dConfirmRes.booking.quoteId) >>= fromMaybeM (QuoteNotFound dConfirmRes.booking.quoteId)
          driver <- runInReplica $ QPerson.findById driverQuote.driverId >>= fromMaybeM (PersonNotFound driverQuote.driverId.getId)
          fork "on_confirm/on_update" $ do
            handle (errHandler dConfirmRes transporter (Just driver)) $ do
              onConfirmMessage <- ACL.buildOnConfirmMessage now dConfirmRes
              void $
                BP.callOnConfirm dConfirmRes.transporter context onConfirmMessage
              void $
                BP.sendRideAssignedUpdateToBAP BP.SIMPLE dConfirmRes.booking ride
          DS.driverScoreEventHandler DST.OnNewRideAssigned {merchantId = transporterId, driverId = driverQuote.driverId}
        DBooking.SpecialZoneBooking -> do
          now <- getCurrentTime
          fork "on_confirm/on_update" $ do
            handle (errHandler' dConfirmRes transporter) $ do
              onConfirmMessage <- ACL.buildOnConfirmMessage now dConfirmRes
              void $
                BP.callOnConfirm dConfirmRes.transporter context onConfirmMessage
    pure Ack
  where
    errHandler dConfirmRes transporter driver exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DConfirm.cancelBooking dConfirmRes.booking driver transporter
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DConfirm.cancelBooking dConfirmRes.booking driver transporter
      | otherwise = throwM exc

    errHandler' dConfirmRes transporter exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DConfirm.cancelBooking dConfirmRes.booking Nothing transporter
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DConfirm.cancelBooking dConfirmRes.booking Nothing transporter
      | otherwise = throwM exc

confirmLockKey :: Text -> Text
confirmLockKey id = "Driver:Confirm:BookingId-" <> id
