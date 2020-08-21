{-# LANGUAGE OverloadedLabels #-}

module Product.Registration (initiateLogin, login, reInitiateLogin) where

import App.Types
import qualified Beckn.External.MyValueFirst.Flow as SF
import qualified Beckn.External.MyValueFirst.Types as SMS
import Beckn.Sms.Config
import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import qualified Crypto.Number.Generate as Cryptonite
import Data.Aeson
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Types.API.Registration
import qualified Utils.Notifications as Notify

initiateLogin :: InitiateLoginReq -> FlowHandler InitiateLoginRes
initiateLogin req =
  withFlowHandler $
    case (req ^. #_medium, req ^. #__type) of
      (SR.SMS, SR.OTP) -> ask >>= initiateFlow req . smsCfg
      _ -> L.throwException $ err400 {errBody = "UNSUPPORTED_MEDIUM_TYPE"}

initiateFlow :: InitiateLoginReq -> SmsConfig -> Flow InitiateLoginRes
initiateFlow req smsCfg = do
  let mobileNumber = req ^. #_mobileNumber
      countryCode = req ^. #_mobileCountryCode
  person <-
    QP.findByMobileNumber countryCode mobileNumber
      >>= maybe (createPerson req) pure
  let entityId = _getPersonId . SP._id $ person
      useFakeOtpM = useFakeSms smsCfg
      scfg = sessionConfig smsCfg
  regToken <- case useFakeOtpM of
    Just _ -> do
      token <- makeSession scfg req entityId SR.USER (show <$> useFakeOtpM)
      QR.create token
      return token
    Nothing -> do
      token <- makeSession scfg req entityId SR.USER Nothing
      QR.create token
      sendOTP (credConfig smsCfg) (countryCode <> mobileNumber) (SR._authValueHash token)
      return token
  let attempts = SR._attempts regToken
      tokenId = SR._id regToken
  Notify.notifyOnRegistration regToken person
  return $ InitiateLoginRes {attempts, tokenId}

makePerson :: InitiateLoginReq -> Flow SP.Person
makePerson req = do
  let role = fromMaybe SP.USER (req ^. #_role)
  pid <- BC.generateGUID
  now <- getCurrTime
  return $
    SP.Person
      { _id = pid,
        _firstName = Nothing,
        _middleName = Nothing,
        _lastName = Nothing,
        _fullName = Nothing,
        _role = role,
        _gender = SP.UNKNOWN,
        _identifierType = SP.MOBILENUMBER,
        _email = Nothing,
        _mobileNumber = Just $ req ^. #_mobileNumber,
        _mobileCountryCode = Just $ req ^. #_mobileCountryCode,
        _identifier = Nothing,
        _rating = Nothing,
        _verified = False,
        _status = SP.INACTIVE,
        _udf1 = Nothing,
        _udf2 = Nothing,
        _deviceToken = req ^. #_deviceToken,
        _organizationId = Nothing,
        _locationId = Nothing,
        _description = Nothing,
        _createdAt = now,
        _updatedAt = now
      }

makeSession ::
  SmsSessionConfig -> InitiateLoginReq -> Text -> SR.RTEntityType -> Maybe Text -> Flow SR.RegistrationToken
makeSession SmsSessionConfig {..} req entityId entityType fakeOtp = do
  otp <- maybe generateOTPCode return fakeOtp
  rtid <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrTime
  return $
    SR.RegistrationToken
      { _id = rtid,
        _token = token,
        _attempts = attempts,
        _authMedium = req ^. #_medium,
        _authType = req ^. #__type,
        _authValueHash = otp,
        _verified = False,
        _authExpiry = authExpiry,
        _tokenExpiry = tokenExpiry,
        _EntityId = entityId,
        _entityType = entityType,
        _createdAt = now,
        _updatedAt = now,
        _info = Nothing
      }

generateOTPCode :: Flow Text
generateOTPCode =
  L.runIO $ padLeft 4 '0' . show <$> Cryptonite.generateBetween 1 9999
  where
    padLeft n c txt =
      let prefix = replicate (max 0 $ n - length txt) c
       in T.pack prefix <> txt

sendOTP :: SmsCredConfig -> Text -> Text -> Flow ()
sendOTP SmsCredConfig {..} phoneNumber otpCode = do
  res <-
    SF.submitSms
      SF.defaultBaseUrl
      SMS.SubmitSms
        { SMS._username = username,
          SMS._password = password,
          SMS._from = SMS.JUSPAY,
          SMS._to = phoneNumber,
          SMS._category = SMS.BULK,
          SMS._text = SF.constructOtpSms otpCode otpHash
        }
  whenLeft res $ \err -> L.throwException err503 {errBody = encode err}

login :: Text -> LoginReq -> FlowHandler LoginRes
login tokenId req =
  withFlowHandler $ do
    SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
    when _verified $ L.throwException $ err400 {errBody = "ALREADY_VERIFIED"}
    checkForExpiry _authExpiry _updatedAt
    let isValid =
          _authMedium == req ^. #_medium && _authType == req ^. #__type
            && _authValueHash
              == req
              ^. #_hash
    if isValid
      then do
        person <- checkPersonExists _EntityId
        QR.updateVerified tokenId True
        let deviceToken = (req ^. #_deviceToken) <|> (person ^. #_deviceToken)
        QP.update (SP._id person) SP.ACTIVE True deviceToken
        updatedPerson <- QP.findPersonById (SP._id person)
        return $ LoginRes _token (Just $ SP.maskPerson updatedPerson)
      else L.throwException $ err400 {errBody = "AUTH_VALUE_MISMATCH"}
  where
    checkForExpiry authExpiry updatedAt =
      whenM (isExpired (realToFrac (authExpiry * 60)) updatedAt) $
        L.throwException $
          err400 {errBody = "AUTH_EXPIRED"}

checkRegistrationTokenExists :: Text -> Flow SR.RegistrationToken
checkRegistrationTokenExists tokenId =
  QR.findRegistrationToken tokenId >>= fromMaybeM400 "INVALID_TOKEN"

createPerson :: InitiateLoginReq -> Flow SP.Person
createPerson req = do
  person <- makePerson req
  QP.create person
  pure person

checkPersonExists :: Text -> Flow SP.Person
checkPersonExists _EntityId =
  QP.findPersonById (PersonId _EntityId)

reInitiateLogin :: Text -> ReInitiateLoginReq -> FlowHandler InitiateLoginRes
reInitiateLogin tokenId req =
  withFlowHandler $ do
    SR.RegistrationToken {..} <- checkRegistrationTokenExists tokenId
    void $ checkPersonExists _EntityId
    if _attempts > 0
      then do
        credCfg <- credConfig . smsCfg <$> ask
        let mobileNumber = req ^. #_mobileNumber
            countryCode = req ^. #_mobileCountryCode
        sendOTP credCfg (countryCode <> mobileNumber) _authValueHash
        _ <- QR.updateAttempts (_attempts - 1) _id
        return $ InitiateLoginRes tokenId (_attempts - 1)
      else L.throwException $ err400 {errBody = "LIMIT_EXCEEDED"}
