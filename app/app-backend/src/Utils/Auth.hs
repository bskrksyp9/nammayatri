module Utils.Auth where

import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Utils.Common as Utils
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.HeaderAuth
import qualified Domain.Types.Person as Person
import qualified Domain.Types.RegistrationToken as SR
import EulerHS.Prelude hiding (id)
import Servant hiding (Context)
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Types.Error

-- | Performs simple token verification.
type TokenAuth = HeaderAuth "token" VerifyToken

data VerifyToken = VerifyToken

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (TokenAuth :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = Id Person.Person
  verificationDescription =
    "Checks whether token is registered.\
    \If you don't have a token, use registration endpoints."

verifyPerson :: (EsqDBFlow m r, HasField "authTokenCacheExpiry" r Seconds) => RegToken -> m (Id Person.Person)
verifyPerson token = do
  let key = authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  mbPersonId <- Redis.getKeyRedis key
  case mbPersonId of
    Just personId -> return personId
    Nothing -> do
      sr <- verifyToken token
      let expiryTime = min sr.tokenExpiry authTokenCacheExpiry
      let personId = Id sr.entityId
      Redis.setExRedis key personId expiryTime
      return personId

authTokenCacheKey :: RegToken -> Text
authTokenCacheKey regToken =
  "BAP:authTokenCacheKey:" <> regToken

verifyPersonAction :: (EsqDBFlow m r, HasField "authTokenCacheExpiry" r Seconds) => VerificationAction VerifyToken m
verifyPersonAction = VerificationAction verifyPerson

verifyToken :: EsqDBFlow m r => RegToken -> m SR.RegistrationToken
verifyToken token =
  RegistrationToken.findByToken token
    >>= Utils.fromMaybeM (InvalidToken token)
    >>= validateToken

validateToken :: EsqDBFlow m r => SR.RegistrationToken -> m SR.RegistrationToken
validateToken sr@SR.RegistrationToken {..} = do
  let nominal = realToFrac $ tokenExpiry * 24 * 60 * 60
  expired <- Utils.isExpired nominal updatedAt
  unless verified $ Utils.throwError TokenIsNotVerified
  when expired $ Utils.throwError TokenExpired
  return sr
