{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module External.Gateway.Flow where

import App.Types
import Beckn.Types.Core.API.Cancel
import Beckn.Types.Core.API.Confirm
import Beckn.Types.Core.API.Feedback
import Beckn.Types.Core.API.Search
import Beckn.Types.Core.API.Status
import Beckn.Types.Core.API.Track
import Beckn.Types.Core.Ack (AckResponse (..), ack)
import Beckn.Types.Core.Error
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManagerKey)
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail, callAPIWithTrail')
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Types as API
import Servant.Client
import Types.API.Location

search ::
  BaseUrl -> SearchReq -> Flow (Either Text ())
search url req = do
  mGatewaySelector <- xGatewaySelector <$> ask
  res <- case mGatewaySelector of
    Just "NSDL.BG.1" -> do
      mNsdlUrl <- xGatewayNsdlUrl <$> ask
      case mNsdlUrl of
        Just nsdlBaseUrl ->
          callAPIWithTrail' (Just signatureAuthManagerKey) nsdlBaseUrl (API.nsdlSearch req) "search"
        Nothing -> throwError500 "invalid nsdl gateway url"
    Just "JUSPAY.BG.1" ->
      callAPIWithTrail' (Just signatureAuthManagerKey) url (API.searchSignAuth req) "search"
    _ -> throwError500 "gateway not configured"
  case res of
    Left err -> do
      L.logError @Text "Search" ("error occurred while search: " <> show err)
      return $ Left $ show err
    Right _ -> do
      L.logInfo @Text "Search" "Search successfully delivered"
      return $ Right ()

confirm :: BaseUrl -> Organization -> ConfirmReq -> Flow AckResponse
confirm url org req@ConfirmReq {context} = do
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  res <- callAPIWithTrail url (API.confirm cbApiKey req) "confirm"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while confirm: " (show err)
  whenLeft res $ \err ->
    L.logError @Text "Confirm" ("error occurred while confirm: " <> show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

location :: BaseUrl -> Text -> Flow (Either Text GetLocationRes)
location url req = do
  res <- callAPIWithTrail url (API.location req) "location"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while confirm: " (show err)
  whenLeft res $ \err ->
    L.logError @Text "Location" ("error occurred while getting location: " <> show err)
  return $ first show res

track :: BaseUrl -> Organization -> TrackTripReq -> Flow AckResponse
track url org req@TrackTripReq {context} = do
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  res <- callAPIWithTrail url (API.trackTrip cbApiKey req) "track"
  case res of
    Left err -> L.logError @Text "error occurred while track trip: " (show err)
    Right _ -> L.logInfo @Text "Track" "Track successfully delivered"
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

cancel :: BaseUrl -> Organization -> CancelReq -> Flow (Either Text ())
cancel url org req = do
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  res <- callAPIWithTrail url (API.cancel cbApiKey req) "cancel"
  case res of
    Left err -> do
      L.logError @Text "error occurred while cancel trip: " (show err)
      return $ Left $ show err
    Right _ -> do
      L.logInfo @Text "Cancel" "Cancel successfully delivered"
      return $ Right ()

status :: BaseUrl -> Organization -> StatusReq -> Flow AckResponse
status url org req@StatusReq {context} = do
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  res <- callAPIWithTrail url (API.status cbApiKey req) "status"
  case res of
    Left err -> L.logError @Text "error occurred while getting status: " (show err)
    Right _ -> L.logInfo @Text "Status" "Status successfully delivered"
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

feedback :: BaseUrl -> Organization -> FeedbackReq -> Flow AckResponse
feedback url org req = do
  let context = req ^. #context
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  res <- callAPIWithTrail url (API.feedback cbApiKey req) "feedback"
  case res of
    Left err -> do
      L.logError @Text "Gateway" $ "Error occurred when sending feedback: " <> show err
      pure $ AckResponse context (ack "ACK") $ Just (domainError $ show err)
    Right _ -> do
      L.logInfo @Text "Gateway" "Feedback successfully sent."
      pure $ AckResponse context (ack "ACK") Nothing
