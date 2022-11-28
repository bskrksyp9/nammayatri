module Domain.Action.UI.Transporter
  ( TransporterRec (..),
    UpdateTransporterReq (..),
    UpdateTransporterRes,
    updateTransporter,
    getTransporter,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id (Id (..))
import Beckn.Types.Predicate
import Beckn.Utils.Common
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import Control.Applicative
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Person as QP
import Tools.Error

newtype TransporterRec = TransporterRec
  { organization :: DM.MerchantAPIEntity
  }
  deriving (Generic, ToJSON, ToSchema)

data UpdateTransporterReq = UpdateTransporterReq
  { name :: Maybe Text,
    description :: Maybe Text,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type UpdateTransporterRes = DM.MerchantAPIEntity

validateUpdateTransporterReq :: Validate UpdateTransporterReq
validateUpdateTransporterReq UpdateTransporterReq {..} =
  sequenceA_
    [ validateField "name" name $ InMaybe $ MinLength 3 `And` P.name,
      validateField "description" description $ InMaybe $ MinLength 3 `And` P.name
    ]

updateTransporter :: (CacheFlow m r, EsqDBFlow m r) => SP.Person -> Id DM.Merchant -> UpdateTransporterReq -> m UpdateTransporterRes
updateTransporter admin merchantId req = do
  unless (Just merchantId == admin.merchantId) $ throwError AccessDenied
  runRequestValidation validateUpdateTransporterReq req
  org <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  let updOrg =
        org{DM.name = fromMaybe (org.name) (req.name),
            DM.description = (req.description) <|> (org.description),
            DM.enabled = fromMaybe (org.enabled) (req.enabled)
           }
  Esq.runTransaction $ CQM.update updOrg
  CQM.clearCache updOrg
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateTransporter : ") (show updOrg)
  return $ DM.makeMerchantAPIEntity updOrg

getTransporter :: (CacheFlow m r, EsqDBFlow m r) => Id SP.Person -> m TransporterRec
getTransporter personId = do
  person <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  case person.merchantId of
    Just merchantId -> TransporterRec . DM.makeMerchantAPIEntity <$> (CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId))
    Nothing -> throwError (PersonFieldNotPresent "merchant_id")
