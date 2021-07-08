module Product.Search
  ( search,
    searchCb,
  )
where

import App.Types
import Beckn.Types.Core.Ack
import Beckn.Types.Error
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Error.BecknAPIError (callBecknAPI')
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..), signatureAuthManagerKey)
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Product.AppLookup as BA
import qualified Product.ProviderRegistry as BP
import Product.Validation
import Servant.Client (showBaseUrl)
import qualified Temporary.Utils as TempUtils
import qualified Types.API.Gateway.Search as ExternalAPI
import Types.API.Search (OnSearchReq, SearchReq (..))
import Types.Beckn.API.Callback
import Types.Error
import Utils.Common

search ::
  SignatureAuthResult Org.Organization ->
  SearchReq ->
  FlowHandler AckResponse
search (SignatureAuthResult proxySign org) req = withFlowHandlerBecknAPI $
  TempUtils.withTransactionIdLogTag req $ do
    validateContext "search" (req.context)
    unless (isJust (req.context.bap_uri)) $
      throwError $ InvalidRequest "No bap URI in context."
    let gatewaySearchSignAuth = ET.client ExternalAPI.searchAPI
        context = req.context
        messageId = context.transaction_id
    cbUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
    cbApiKey <- org.callbackApiKey & fromMaybeM (OrgFieldNotPresent "callback_api_key")
    providers <- BP.lookup context
    let bgSession = BA.GwSession cbUrl cbApiKey context
    BA.insert messageId bgSession
    when (null providers) $ throwError NoProviders
    forM_ providers $ \provider -> fork "Provider search" . withLogTag "search_req" $ do
      providerUrl <- provider.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url") -- Already checked for existance
      withLogTag ("providerUrl_" <> T.pack (showBaseUrl providerUrl)) do
        -- TODO maybe we should explicitly call sign request here instead of using callAPIWithTrail'?
        callBecknAPI'
          (Just signatureAuthManagerKey)
          Nothing
          providerUrl
          (gatewaySearchSignAuth (Just proxySign) req)
          "search"
    return Ack

searchCb ::
  SignatureAuthResult Org.Organization ->
  OnSearchReq ->
  FlowHandler AckResponse
searchCb (SignatureAuthResult proxySign org) req@CallbackReq {context} = withFlowHandlerBecknAPI $
  TempUtils.withTransactionIdLogTag req . withLogTag "search_cb" $ do
    providerUrl <- org.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
    withLogTag ("providerUrl_" <> T.pack (showBaseUrl providerUrl)) do
      validateContext "on_search" context
      let gatewayOnSearchSignAuth = ET.client ExternalAPI.onSearchAPI
          messageId = req.context.transaction_id
      bgSession <- BA.lookup messageId >>= fromMaybeM (InvalidRequest "Message not found.")
      let baseUrl = bgSession.cbUrl
      callBecknAPI'
        (Just signatureAuthManagerKey)
        Nothing
        baseUrl
        (gatewayOnSearchSignAuth (Just proxySign) req)
        "on_search"
      return Ack
