{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.RideBooking.Registration where

import qualified "rider-app" API.Dashboard.RideBooking.Registration as BAP
import qualified Dashboard.RiderPlatform.Customer as Common
import qualified "rider-app" Domain.Action.UI.Registration as DR
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "rider-app" Domain.Types.Person as SP
import qualified "rider-app" Domain.Types.RegistrationToken as DTR
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp as Client
import Servant
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant (merchantAccessCheck)

type API =
  "registration"
    :> ( ApiAuth 'APP_BACKEND 'CUSTOMERS 'AUTH
           :> BAP.CustomerAuthAPI
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'VERIFY
             :> BAP.CustomerVerify
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'RESEND
             :> BAP.CustomerResend
           :<|> ApiAuth 'APP_BACKEND 'CUSTOMERS 'LOGOUT
             :> BAP.CustomerLogout
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  callAuth merchantId
    :<|> callVerify merchantId
    :<|> callResend merchantId
    :<|> callLogout merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  BAP.RegistrationEndPoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.RegistrationAPI endpoint) (Just APP_BACKEND) (Just apiTokenInfo) Nothing Nothing

callAuth :: ShortId DM.Merchant -> ApiTokenInfo -> BAP.CustomerAuthReq -> FlowHandler DR.AuthRes
callAuth merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction BAP.RegistrationAuthEndPoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.rideBooking.registration.auth) req

callVerify :: ShortId DM.Merchant -> ApiTokenInfo -> Id DTR.RegistrationToken -> DR.AuthVerifyReq -> FlowHandler DR.AuthVerifyRes
callVerify merchantShortId apiTokenInfo tokenId req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction BAP.RegistrationVerifyEndPoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.rideBooking.registration.verify) tokenId req

callResend :: ShortId DM.Merchant -> ApiTokenInfo -> Id DTR.RegistrationToken -> FlowHandler DR.ResendAuthRes
callResend merchantShortId apiTokenInfo tokenId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction BAP.RegistrationResendEndPoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.rideBooking.registration.resend) tokenId

callLogout :: ShortId DM.Merchant -> ApiTokenInfo -> Id SP.Person -> FlowHandler APISuccess
callLogout merchantShortId apiTokenInfo personId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction BAP.RegistrationLogoutEndPoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callRiderApp checkedMerchantId (.rideBooking.registration.logout) personId
