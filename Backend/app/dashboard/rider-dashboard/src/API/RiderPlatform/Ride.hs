{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.Ride
  ( API,
    handler,
  )
where

import qualified "rider-app" API.Dashboard.MultipleRideCancel as BAP'
import qualified "rider-app" API.Dashboard.MultipleRideEnd as BAP
import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Ride as Common
import qualified "rider-app" Domain.Action.Dashboard.MultipleRideCancel as Domain
import qualified "rider-app" Domain.Action.Dashboard.MultipleRideEnd as Domain
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import qualified RiderPlatformClient.RiderApp as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (BECKN_TRANSPORT, DRIVER_OFFER_BPP)
import Tools.Auth.Merchant

type API =
  "ride"
    :> ( ShareRideInfoAPI
           :<|> RideListAPI
           :<|> MultipleRideEndAPI
           :<|> MultipleRideCancelAPI
       )

type RideListAPI = Common.RideListAPI

type ShareRideInfoAPI = Common.ShareRideInfoAPI

type MultipleRideEndAPI =
  ApiAuth 'APP_BACKEND 'RIDES 'MULTIPLE_RIDE_END -- 'WRITE_ACCESS
    :> BAP.API

type MultipleRideCancelAPI =
  ApiAuth 'APP_BACKEND 'RIDES 'MULTIPLE_RIDE_CANCEL -- 'WRITE_ACCESS
    :> BAP'.API

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  shareRideInfo merchantId
    :<|> rideList merchantId
    :<|> multipleRideEnd merchantId
    :<|> multipleRideCancel merchantId

rideInfoHitsCountKey :: Id Common.Ride -> Text
rideInfoHitsCountKey rideId = "RideInfoHits:" <> getId rideId <> ":hitsCount"

buildTransactionMultipleRide ::
  ( MonadFlow m,
    Domain.HideSecrets request
  ) =>
  BAP.MultipleRideEndEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransactionMultipleRide endpoint apiTokenInfo =
  T.buildTransaction (DT.MultipleRideEndAPI endpoint) (Just APP_BACKEND) (Just apiTokenInfo) Nothing Nothing

buildTransactionMultipleRide' ::
  ( MonadFlow m,
    Domain.HideSecrets request
  ) =>
  BAP'.MultipleRideCancelEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransactionMultipleRide' endpoint apiTokenInfo =
  T.buildTransaction (DT.MultipleRideCancelAPI endpoint) (Just APP_BACKEND) (Just apiTokenInfo) Nothing Nothing

shareRideInfo ::
  ShortId DM.Merchant ->
  Id Common.Ride ->
  FlowHandler Common.ShareRideInfoRes
shareRideInfo merchantShortId rideId = withFlowHandlerAPI $ do
  shareRideApiRateLimitOptions <- asks (.shareRideApiRateLimitOptions)
  checkSlidingWindowLimitWithOptions (rideInfoHitsCountKey rideId) shareRideApiRateLimitOptions
  checkedMerchantId <- merchantAccessCheck merchantShortId merchantShortId
  Client.callRiderApp checkedMerchantId (.rides.shareRideInfo) rideId

rideList ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  FlowHandler Common.RideListRes
rideList merchantShortId mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone =
  withFlowHandlerAPI $ do
    checkedMerchantId <- merchantAccessCheck merchantShortId merchantShortId
    Client.callRiderApp checkedMerchantId (.rides.rideList) mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone

multipleRideEnd :: ShortId DM.Merchant -> ApiTokenInfo -> Domain.MultipleRideEndReq -> FlowHandler APISuccess
multipleRideEnd merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransactionMultipleRide BAP.MultipleRideEndEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.multipleRideEnds.multipleRideEnd) req

multipleRideCancel :: ShortId DM.Merchant -> ApiTokenInfo -> Domain.MultipleRideCancelReq -> FlowHandler APISuccess
multipleRideCancel merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransactionMultipleRide' BAP'.MultipleRideCancelEndpoint apiTokenInfo (Just req)
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.multipleRideCancels.multipleRideCancel) req
