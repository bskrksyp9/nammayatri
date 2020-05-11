module Product.Person where

import EulerHS.Prelude
import qualified Storage.Queries.Person                  as QP
import qualified Storage.Queries.RegistrationToken       as QR
import qualified Beckn.Types.Storage.RegistrationToken   as SR
import qualified Beckn.Types.Storage.Person              as SP
import Types.App
import Beckn.Types.App
import Types.API.Person
import Beckn.Utils.Common
import Utils.Routes
import Beckn.TypeClass.Transform

updatePerson :: Text -> UpdatePersonReq -> FlowHandler UpdatePersonRes
updatePerson regToken req = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.findRegistrationTokenByToken regToken
  person                    <- QP.findPersonById (PersonId _EntityId)
  let updatedPerson           = transform req person
  QP.updatePersonRec (PersonId _EntityId) updatedPerson
  return $ UpdatePersonRes updatedPerson