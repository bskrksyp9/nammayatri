{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : FCM.Flow
-- Description : Firebase Cloud Messaging module
--
-- FCM description: https://firebase.google.com/docs/cloud-messaging
-- Firebase Cloud Messaging (FCM) is a cross-platform messaging solution
-- that lets you reliably send messages at no cost. Using FCM, you can notify
-- a client app that new email or other data is available to sync. You can
-- send notification messages to drive user re-engagement and retention.
-- For use cases such as instant messaging, a message can transfer
-- a payload of up to 4KB to a client app.
--
-- Protocol description : https://firebase.google.com/docs/reference/fcm/rest/v1/projects.messages
module Beckn.External.FCM.Flow where

import Beckn.External.FCM.Types
import Beckn.Types.App (_getPersonId)
import Beckn.Types.Common
import Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common (fork)
import qualified Beckn.Utils.JWT as JWT
import Beckn.Utils.Logging (HasLogContext, Log (..))
import Control.Lens
import Data.Default.Class
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding ((^.))
import qualified EulerHS.Types as ET
import GHC.Records (HasField (..))
import Servant
import Servant.Client

-- | Create FCM message
-- Note that data should be formed as key-value pairs list
-- recipientId::FCMToken is an app's registration token
createMessage :: FCMNotificationTitle -> FCMNotificationBody -> FCMData -> FCMRecipientToken -> FCMMessage
createMessage title body msgData recipientId =
  def & fcmToken ?~ recipientId
    & fcmData ?~ msgData
    & fcmAndroid ?~ androidCfg
  where
    tag = msgData ^. fcmNotificationType
    androidCfg = createAndroidConfig title body tag

-- | Android Notification details
createAndroidConfig :: FCMNotificationTitle -> FCMNotificationBody -> FCMNotificationType -> FCMAndroidConfig
createAndroidConfig title body tag =
  def & fcmdNotification ?~ notification
  where
    notification =
      def & fcmdTitle ?~ title
        & fcmdBody ?~ body
        & fcmdIcon
          ?~ FCMNotificationIconUrl
            "https://api.sandbox.beckn.juspay.in/static/images/ride-success.png"
        & fcmdTag ?~ tag

-- | Send FCM message to a person
notifyPerson ::
  (HasLogContext r, HasField "fcmUrl" r BaseUrl) =>
  FCMNotificationTitle ->
  FCMNotificationBody ->
  FCMData ->
  Person ->
  FlowR r ()
notifyPerson title body msgData person =
  let pid = _getPersonId $ Person._id person
      tokenNotFound = "device token of a person " <> pid <> " not found"
   in case Person._deviceToken person of
        Nothing -> do
          logInfo (T.pack "FCM") tokenNotFound
          pure ()
        Just token ->
          sendMessage (FCMRequest (createMessage title body msgData token)) pid

-- | Google API interface
type FCMSendMessageAPI =
  Header "Authorization" FCMAuthToken
    :> ReqBody '[JSON] FCMRequest
    :> Post '[JSON] FCMResponse

fcmSendMessageAPI :: Proxy FCMSendMessageAPI
fcmSendMessageAPI = Proxy

-- | Send FCM message to a registered device
sendMessage ::
  (HasLogContext r, HasField "fcmUrl" r BaseUrl) =>
  FCMRequest ->
  T.Text ->
  FlowR r ()
sendMessage fcmMsg toWhom = fork desc $ do
  authToken <- getTokenText
  case authToken of
    Right token -> do
      fcmUrl <- getField @"fcmUrl" <$> ask
      res <- L.callAPI fcmUrl $ callFCM (Just $ FCMAuthToken token) fcmMsg
      logInfo fcm $ case res of
        Right _ -> "message sent successfully to a person with id = " <> toWhom
        Left x -> "error: " <> show x
      pure ()
    Left err -> do
      logError fcm $ "error: " <> show err
      pure ()
  where
    callFCM token msg = void $ ET.client fcmSendMessageAPI token msg
    desc = "FCM send message forked flow"
    fcm = T.pack "FCM"

-- | try to get FCM text token
getTokenText :: (L.MonadFlow m, Log m) => m (Either String Text)
getTokenText = do
  token <- getToken
  pure $ case token of
    Left err -> Left err
    Right t -> Right $ JWT._jwtTokenType t <> T.pack " " <> JWT._jwtAccessToken t

-- | check FCM token and refresh if it is invalid
checkAndGetToken ::
  (L.MonadFlow m, Log m) =>
  JWT.ServiceAccount ->
  m (Either String JWT.JWToken)
checkAndGetToken sa = do
  token <- L.getOption FCMTokenKey
  case token of
    Nothing -> refreshToken
    Just t -> do
      validityStatus <- L.runIO $ JWT.isValid t
      case validityStatus of
        JWT.JWTValid x ->
          if x > 300
            then pure $ Right t -- do nothing, token is ok
            else do
              -- close to expiration, start trying to refresh token
              logInfo fcm "Token is about to be expiried, trying to refresh it"
              refreshToken
        JWT.JWTExpired x -> do
          -- token expired
          logInfo fcm $ "Token expired " <> show x <> " seconds ago, trying to refresh it"
          L.delOption FCMTokenKey
          refreshToken
        JWT.JWTInvalid -> do
          -- token is invalid
          logInfo fcm "Token is invalid, trying to refresh it"
          L.delOption FCMTokenKey
          refreshToken
  where
    fcm = T.pack "FCM"
    refreshToken = do
      logInfo fcm "Refreshing token"
      t <- L.runIO $ JWT.doRefreshToken sa
      case t of
        Left err -> do
          logInfo fcm $ T.pack err
          pure $ Left err
        Right token -> do
          logInfo fcm $ T.pack "Success"
          L.setOption FCMTokenKey token
          pure $ Right token

-- | Get token (do not refresh it if it is expired / invalid)
getToken :: L.MonadFlow m => m (Either String JWT.JWToken)
getToken = do
  token <- L.getOption FCMTokenKey
  case token of
    Nothing -> pure $ Left "Token not found"
    Just t -> do
      validityStatus <- L.runIO $ JWT.isValid t
      pure $ case validityStatus of
        JWT.JWTValid _ -> Right t
        JWT.JWTExpired _ -> Left "Token expired"
        JWT.JWTInvalid -> Left "Token is invalid"
