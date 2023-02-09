module API.BPP.DriverOffer.Ride
  ( API,
    handler,
  )
where

import qualified BPPClient.DriverOffer as Client
import qualified "dashboard-bpp-helper-api" Dashboard.BPP.Ride as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI)
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (BECKN_TRANSPORT)
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "ride"
    :> ( RideListAPI
           :<|> RideStartAPI
           :<|> RideEndAPI
           :<|> RideCancelAPI
           :<|> RideInfoAPI
           :<|> RideSyncAPI
       )

type RideListAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'RIDES
    :> Common.RideListAPI

type RideStartAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'RIDES
    :> Common.RideStartAPI

type RideEndAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'RIDES
    :> Common.RideEndAPI

type RideCancelAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'RIDES
    :> Common.RideCancelAPI

type RideInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'RIDES
    :> Common.RideInfoAPI

type RideSyncAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'RIDES
    :> Common.RideSyncAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  rideList merchantId
    :<|> rideStart merchantId
    :<|> rideEnd merchantId
    :<|> rideCancel merchantId
    :<|> rideInfo merchantId
    :<|> rideSync merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.RideEndpoint ->
  ApiTokenInfo ->
  Id Common.Ride ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo rideId =
  T.buildTransaction (DT.RideAPI endpoint) apiTokenInfo Nothing (Just rideId)

rideList ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  FlowHandler Common.RideListRes
rideList merchantShortId apiTokenInfo mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.rides.rideList) mbLimit mbOffset mbBookingStatus mbShortRideId mbCustomerPhone mbDriverPhone

rideStart :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> Common.StartRideReq -> FlowHandler APISuccess
rideStart merchantShortId apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideStartEndpoint apiTokenInfo rideId (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.rides.rideStart) rideId req

rideEnd :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> Common.EndRideReq -> FlowHandler APISuccess
rideEnd merchantShortId apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideEndEndpoint apiTokenInfo rideId (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.rides.rideEnd) rideId req

rideCancel :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> Common.CancelRideReq -> FlowHandler APISuccess
rideCancel merchantShortId apiTokenInfo rideId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideCancelEndpoint apiTokenInfo rideId (Just req)
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.rides.rideCancel) rideId req

rideInfo :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> FlowHandler Common.RideInfoRes
rideInfo merchantShortId apiTokenInfo rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.rides.rideInfo) rideId

rideSync :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Ride -> FlowHandler Common.RideSyncRes
rideSync merchantShortId apiTokenInfo rideId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.RideSyncEndpoint apiTokenInfo rideId T.emptyRequest
  T.withResponseTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.rides.rideSync) rideId