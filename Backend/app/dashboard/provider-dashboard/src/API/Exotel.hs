{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Exotel
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.Common.Exotel as Common
import qualified Domain.Types.ServerName as DSN
import qualified Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess
import Kernel.Utils.Common (MonadFlow, decodeFromText, fork, withFlowHandlerAPI)
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import qualified ProviderPlatformClient.StaticOfferDriver as Client
import qualified RiderPlatformClient.RiderApp as Client
import Servant
import qualified SharedLogic.Transaction as T
import qualified Storage.Queries.Transaction as QT

type API =
  "exotel"
    :> Common.ExotelHeartbeatAPI

handler :: FlowServer API
handler =
  exotelHeartbeat

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.ExotelEndpoint ->
  DSN.ServerName ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint serverName =
  T.buildTransaction (DT.ExotelAPI endpoint) (Just serverName) Nothing Nothing Nothing

-- store request and call bap/bpp only when status changed OK to not OK and vice versa.
exotelHeartbeat :: Common.ExotelHeartbeatReq -> FlowHandler APISuccess
exotelHeartbeat req = withFlowHandlerAPI $ do
  forM_ [DSN.APP_BACKEND, DSN.BECKN_TRANSPORT, DSN.DRIVER_OFFER_BPP] $ \serverName -> do
    fork ("exotelHeartbeat:" <> show serverName) $ do
      needToCallApp <-
        if req.statusType /= Common.OK
          then pure True
          else do
            mbLastTransaction <- Esq.runInReplica $ QT.fetchLastTransaction (DT.ExotelAPI Common.ExotelHeartbeatEndpoint) serverName
            let mbLastStatus =
                  mbLastTransaction
                    >>= (.request)
                    >>= decodeFromText @(Common.ReqWithoutSecrets Common.ExotelHeartbeatReq)
                    <&> (.statusType)
            let lastTransactionFailed =
                  mbLastTransaction
                    >>= (.responseError)
                    & isJust
            pure (mbLastStatus /= Just Common.OK || lastTransactionFailed)
      when needToCallApp $ do
        transaction <- buildTransaction Common.ExotelHeartbeatEndpoint serverName (Just req)
        T.withTransactionStoring transaction $
          void $ callExotelHeartbeat serverName
  pure Success
  where
    callExotelHeartbeat DSN.APP_BACKEND = Client.callRiderAppExotelApi (.exotelHeartbeat) req
    callExotelHeartbeat DSN.BECKN_TRANSPORT = Client.callStaticOfferDriverAppExotelApi (.exotelHeartbeat) req
    callExotelHeartbeat DSN.DRIVER_OFFER_BPP = Client.callDynamicOfferDriverAppExotelApi (.exotelHeartbeat) req
