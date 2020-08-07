{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Dunzo.Flow where

import App.Types
import Beckn.Types.App (CaseId (..))
import Beckn.Types.Common (AckResponse (..), ack)
import Beckn.Types.Core.Context
import Beckn.Types.FMD.API.Cancel (CancelReq, CancelRes, onCancelAPI)
import Beckn.Types.FMD.API.Confirm (ConfirmReq, ConfirmRes)
import Beckn.Types.FMD.API.Init (InitReq, InitRes, onInitAPI)
import Beckn.Types.FMD.API.Search (SearchReq, SearchRes, onSearchAPI)
import Beckn.Types.FMD.API.Select (DraftOrder (..), SelectReq (..), SelectRes, onSelectAPI)
import Beckn.Types.FMD.API.Status (StatusReq, StatusRes, onStatusAPI)
import Beckn.Types.FMD.API.Track (TrackReq, TrackRes, onTrackAPI)
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (decodeFromText, encodeToText, fromMaybeM400, fromMaybeM500, throwJsonError500)
import Beckn.Utils.Extra (getCurrentTimeUTC)
import Control.Lens ((?~))
import Control.Lens.Combinators hiding (Context)
import Data.Aeson
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Dunzo.Flow as API
import External.Dunzo.Types
import Product.Dunzo.Transform
import Servant.Client (ClientError (..), ResponseF (..))
import qualified Storage.Queries.Case as Storage
import qualified Storage.Queries.Dunzo as Dz
import qualified Storage.Queries.Quote as Storage
import Types.Wrapper
import Utils.Common (fork, parseBaseUrl)

search :: Organization -> SearchReq -> Flow SearchRes
search org req = do
  config@DunzoConfig {..} <- getDunzoConfig org
  quoteReq <- mkQuoteReqFromSearch req
  fork "Search" $ do
    eres <- getQuote config quoteReq
    L.logInfo @Text "QuoteRes" $ show eres
    case eres of
      Left err ->
        case err of
          FailureResponse _ (Response _ _ _ body) -> sendErrCb org dzBPId dzBPNwAddress body
          _ -> L.logDebug @Text "getQuoteErr" (show err)
      Right res -> sendCb org dzBPId dzBPNwAddress res
  returnAck config (req ^. #context)
  where
    sendCb org' dzBPId dzBPNwAddress res = do
      onSearchReq <- mkOnSearchReq org' (updateContext (req ^. #context) dzBPId dzBPNwAddress) res
      cbUrl <- org' ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED" >>= parseBaseUrl
      cbApiKey <- org' ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
      cbres <- callCbAPI cbApiKey cbUrl onSearchReq
      L.logDebug @Text "cb" $
        decodeUtf8 (encode onSearchReq)
          <> show cbres

    callCbAPI cbApiKey cbUrl = L.callAPI cbUrl . ET.client onSearchAPI cbApiKey

    sendErrCb org' dzBPId dzBPNwAddress errbody =
      case decode errbody of
        Just err -> do
          cbUrl <- org' ^. #_callbackUrl & fromMaybeM500 "CB_URL_NOT_CONFIGURED" >>= parseBaseUrl
          cbApiKey <- org' ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
          onSearchErrReq <- mkOnSearchErrReq org' (updateContext (req ^. #context) dzBPId dzBPNwAddress) err
          cbres <- callCbAPI cbApiKey cbUrl onSearchErrReq
          L.logDebug @Text "cb" $
            decodeUtf8 (encode onSearchErrReq)
              <> show cbres
        Nothing -> L.logDebug @Text "getQuoteErr" "UNABLE_TO_DECODE_ERR"

select :: Organization -> SelectReq -> Flow SelectRes
select org req = do
  config@DunzoConfig {..} <- getDunzoConfig org
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  let maybeBaConfig = find (\x -> req ^. #context . #_bap_id == Just (x ^. #bap_id)) dzBAConfigs
  baUrl <-
    fromMaybeM500 "CB_URL_NOT_CONFIGURED" $
      (maybeBaConfig ^? _Just . #bap_nw_address) <|> (req ^. #context . #_bap_nw_address)
  cbUrl <- parseBaseUrl baUrl
  fork "Select" do
    quoteReq <- mkQuoteReqFromSelect req
    eres <- getQuote config quoteReq
    L.logInfo @Text "QuoteRes" $ show eres
    sendCallback req eres cbUrl cbApiKey
  returnAck config (req ^. #context)
  where
    sendCallback req' (Right res) cbUrl cbApiKey = do
      quote <- mkQuote res
      let onSelectMessage = mkOnSelectMessage quote req'
      let onSelectReq = mkOnSelectReq req' onSelectMessage
      let quoteId = quote ^. #_id
      let order = onSelectMessage ^. #order
      let orderDetails = OrderDetails order quote
      Storage.storeQuote quoteId orderDetails
      L.logInfo @Text "on_select" $ "on_select cb req" <> show onSelectReq
      onSelectResp <- L.callAPI cbUrl $ ET.client onSelectAPI cbApiKey onSelectReq
      L.logInfo @Text "on_select" $ "on_select cb resp" <> show onSelectResp
      return ()
    sendCallback req' (Left (FailureResponse _ (Response _ _ _ body))) cbUrl cbApiKey =
      case decode body of
        Just err -> do
          let onSelectReq = mkOnSelectErrReq req' err
          L.logInfo @Text "on_select" $ "on_select cb err req" <> show onSelectReq
          onSelectResp <- L.callAPI cbUrl $ ET.client onSelectAPI cbApiKey onSelectReq
          L.logInfo @Text "on_select" $ "on_select cb err resp" <> show onSelectResp
          return ()
        Nothing -> return ()
    sendCallback _ _ _ _ = return ()

init :: Organization -> InitReq -> Flow InitRes
init org req = do
  conf@DunzoConfig {..} <- getDunzoConfig org
  let context = req ^. #context
  let quoteId = req ^. (#message . #quotation_id)
  bapNwAddr <- req ^. (#context . #_bap_nw_address) & fromMaybeM400 "INVALID_BAP_NW_ADDR"
  baConfig <- getBAConfig bapNwAddr conf
  orderDetails <- Storage.lookupQuote quoteId >>= fromMaybeM400 "INVALID_QUOTATION_ID"
  fork "init" do
    quoteReq <- mkQuoteReqFromSelect $ SelectReq context (DraftOrder (orderDetails ^. #order))
    eres <- getQuote conf quoteReq
    L.logInfo @Text "QuoteRes" $ show eres
    sendCb orderDetails req baConfig eres
  returnAck conf (req ^. #context)
  where
    callCbAPI cbApiKey cbUrl = L.callAPI cbUrl . ET.client onInitAPI cbApiKey

    sendCb orderDetails req' baConfig (Right res) = do
      let quoteId = req' ^. (#message . #quotation_id)
          cbApiKey = baConfig ^. #bap_api_key
      cbUrl <- parseBaseUrl $ baConfig ^. #bap_nw_address
      -- quoteId will be used as orderId
      let onInitMessage =
            mkOnInitMessage
              quoteId
              (orderDetails ^. #order)
              (baConfig ^. #paymentPolicy)
              req'
              res
      let onInitReq = mkOnInitReq req' onInitMessage
      createCase (onInitMessage ^. #order) (orderDetails ^. #quote)
      onInitResp <- callCbAPI cbApiKey cbUrl onInitReq
      L.logInfo @Text "on_init" $ show onInitResp
      return ()
    sendCb _ req' baConfig (Left (FailureResponse _ (Response _ _ _ body))) = do
      let cbApiKey = baConfig ^. #bap_api_key
      cbUrl <- parseBaseUrl $ baConfig ^. #bap_nw_address
      case decode body of
        Just err -> do
          onInitReq <- mkOnInitErrReq req' err
          onInitResp <- callCbAPI cbApiKey cbUrl onInitReq
          L.logInfo @Text "on_init err" $ show onInitResp
          return ()
        Nothing -> return ()
    sendCb _ _ _ _ = return ()

    createCase order quote = do
      now <- getCurrentTimeUTC
      let case_ =
            Case
              { _id = CaseId $ fromJust $ order ^. #_id,
                _name = Nothing,
                _description = Nothing,
                _shortId = "",
                _industry = GROCERY,
                _type = RIDEORDER,
                _exchangeType = ORDER,
                _status = NEW,
                _startTime = now,
                _endTime = Nothing,
                _validTill = now,
                _provider = Nothing,
                _providerType = Nothing,
                _requestor = Nothing,
                _requestorType = Nothing,
                _parentCaseId = Nothing,
                _fromLocationId = "",
                _toLocationId = "",
                _udf1 = Just $ encodeToText (OrderDetails order quote),
                _udf2 = Nothing,
                _udf3 = Nothing,
                _udf4 = Nothing,
                _udf5 = Nothing,
                _info = Nothing,
                _createdAt = now,
                _updatedAt = now
              }
      Storage.create case_

confirm :: Organization -> ConfirmReq -> Flow ConfirmRes
confirm _ _ = error "Not implemented yet"

track :: Organization -> TrackReq -> Flow TrackRes
track org req = do
  let orderId = req ^. (#message . #order_id)
  conf@DunzoConfig {..} <- getDunzoConfig org
  bapNwAddr <- req ^. (#context . #_bap_nw_address) & fromMaybeM400 "INVALID_BAP_NW_ADDR"
  baConfig <- getBAConfig bapNwAddr conf
  void $ Storage.findById (CaseId orderId) >>= fromMaybeM400 "ORDER_NOT_FOUND"
  fork "track" do
    -- TODO: fix this after dunzo sends tracking url in api
    onTrackReq <- mkOnTrackErrReq req
    eres <- callCbAPI baConfig onTrackReq
    L.logInfo @Text "on_track" $ show eres
  returnAck conf (req ^. #context)
  where
    callCbAPI baConfig req' = do
      let cbApiKey = baConfig ^. #bap_api_key
      cbUrl <- parseBaseUrl $ baConfig ^. #bap_nw_address
      L.callAPI cbUrl $ ET.client onTrackAPI cbApiKey req'

status :: Organization -> StatusReq -> Flow StatusRes
status org req = do
  conf@DunzoConfig {..} <- getDunzoConfig org
  let orderId = req ^. (#message . #order_id)
  bapNwAddr <- req ^. (#context . #_bap_nw_address) & fromMaybeM400 "INVALID_BAP_NW_ADDR"
  baConfig <- getBAConfig bapNwAddr conf
  c <- Storage.findById (CaseId orderId) >>= fromMaybeM400 "ORDER_NOT_FOUND"
  let taskId = c ^. #_shortId
  (orderDetails :: OrderDetails) <- c ^. #_udf1 >>= decodeFromText & fromMaybeM500 "ORDER_NOT_FOUND"
  fork "status" do
    eres <- getStatus conf (TaskId taskId)
    L.logInfo @Text "StatusRes" $ show eres
    sendCb c orderDetails req baConfig eres
  returnAck conf (req ^. #context)
  where
    getStatus conf@DunzoConfig {..} taskId = do
      baseUrl <- parseBaseUrl dzUrl
      token <- fetchToken conf
      API.taskStatus dzClientId token baseUrl taskId

    updateCase caseId orderDetails taskStatus case_ = do
      let updatedCase = case_ {_udf1 = Just $ encodeToText orderDetails, _udf2 = Just $ encodeToText taskStatus}
      Storage.update caseId updatedCase

    callCbAPI cbApiKey cbUrl = L.callAPI cbUrl . ET.client onStatusAPI cbApiKey

    sendCb case_ orderDetails req' baConfig res = do
      let cbApiKey = baConfig ^. #bap_api_key
      let order = orderDetails ^. #order
      cbUrl <- parseBaseUrl $ baConfig ^. #bap_nw_address
      case res of
        Right taskStatus -> do
          onStatusMessage <- mkOnStatusMessage (org ^. #_name) order taskStatus
          onStatusReq <- mkOnStatusReq req' onStatusMessage
          let updatedOrder = onStatusMessage ^. #order
          let updatedOrderDetails = orderDetails & #order .~ updatedOrder
          updateCase (case_ ^. #_id) updatedOrderDetails taskStatus case_
          onStatusRes <- callCbAPI cbApiKey cbUrl onStatusReq
          L.logInfo @Text "on_status " $ show onStatusRes
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              onStatusReq <- mkOnStatusErrReq req' err
              onStatusResp <- callCbAPI cbApiKey cbUrl onStatusReq
              L.logInfo @Text "on_status err " $ show onStatusResp
        _ -> pass

cancel :: Organization -> CancelReq -> Flow CancelRes
cancel org req = do
  let oId = req ^. (#message . #order_id)
  conf@DunzoConfig {..} <- getDunzoConfig org
  bapNwAddr <- req ^. (#context . #_bap_nw_address) & fromMaybeM400 "INVALID_BAP_NW_ADDR"
  bapConfig <- getBAConfig bapNwAddr conf
  case_ <- Storage.findById (CaseId oId) >>= fromMaybeM400 "ORDER_NOT_FOUND"
  let taskId = case_ ^. #_shortId
  orderDetails <- case_ ^. #_udf1 >>= decodeFromText & fromMaybeM500 "ORDER_NOT_FOUND"
  fork "cancel" do
    eres <- callCancelAPI conf (TaskId taskId)
    L.logInfo @Text "CancelRes" $ show eres
    sendCb case_ orderDetails req bapConfig eres
  returnAck conf (req ^. #context)
  where
    callCancelAPI conf@DunzoConfig {..} taskId = do
      baseUrl <- parseBaseUrl dzUrl
      token <- fetchToken conf
      -- TODO get cancellation reason
      API.cancelTask dzClientId token baseUrl taskId ""

    updateCase :: CaseId -> OrderDetails -> Case -> Flow ()
    updateCase caseId orderDetails case_ = do
      let updatedOrderDetails = orderDetails & (#order . #_state) ?~ "CANCELLED"
          updatedCase = case_ {_udf1 = Just $ encodeToText updatedOrderDetails}
      Storage.update caseId updatedCase

    callCbAPI baConfig req' = do
      let cbApiKey = baConfig ^. #bap_api_key
      cbUrl <- parseBaseUrl $ baConfig ^. #bap_nw_address
      L.callAPI cbUrl $ ET.client onCancelAPI cbApiKey req'

    sendCb case_ orderDetails req' baConfig res =
      case res of
        Right () -> do
          let order = orderDetails ^. #order
          onCancelReq <- mkOnCancelReq req' order
          updateCase (case_ ^. #_id) orderDetails case_
          onCancelRes <- callCbAPI baConfig onCancelReq
          L.logInfo @Text "on_cancel " $ show onCancelRes
        Left (FailureResponse _ (Response _ _ _ body)) ->
          whenJust (decode body) handleError
          where
            handleError err = do
              onCancelReq <- mkOnCancelErrReq req' err
              onCancelResp <- callCbAPI baConfig onCancelReq
              L.logInfo @Text "on_cancel err " $ show onCancelResp
        _ -> pass

getQuote :: DunzoConfig -> QuoteReq -> Flow (Either ClientError QuoteRes)
getQuote conf@DunzoConfig {..} quoteReq = do
  baseUrl <- parseBaseUrl dzUrl
  token <- fetchToken conf
  API.getQuote dzClientId token baseUrl quoteReq

fetchToken :: DunzoConfig -> Flow Token
fetchToken DunzoConfig {..} = do
  tokenUrl <- parseBaseUrl "http://d4b.dunzodev.in:9016" -- TODO: Fix this, should not be hardcoded
  mToken <- Dz.getToken
  case mToken of
    Nothing -> do
      eres <- API.getToken tokenUrl (TokenReq dzClientId dzClientSecret)
      case eres of
        Left err -> throwJsonError500 "TOKEN_ERR" (show err)
        Right (TokenRes token) -> do
          Dz.insertToken token
          return token
    Just token -> return token

getBAConfig :: Text -> DunzoConfig -> Flow BAConfig
getBAConfig bapNwAddr DunzoConfig {..} =
  find (\c -> c ^. #bap_nw_address == bapNwAddr) dzBAConfigs
    & fromMaybeM500 "BAP_NOT_CONFIGURED"

returnAck :: DunzoConfig -> Context -> Flow AckResponse
returnAck DunzoConfig {..} context = return $ AckResponse (updateContext context dzBPId dzBPNwAddress) (ack "ACK") Nothing
