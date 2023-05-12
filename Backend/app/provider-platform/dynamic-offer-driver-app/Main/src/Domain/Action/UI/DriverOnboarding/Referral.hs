{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.Referral where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import Environment
import Kernel.External.Encryption (encrypt)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Types.Validation (Validate)
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation (runRequestValidation, validateField)
import qualified Storage.CachedQueries.DriverInformation as DriverInformation

newtype ReferralReq = ReferralReq
  {value :: Text}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

type ReferralRes = APISuccess

validateReferralReq :: Validate ReferralReq
validateReferralReq ReferralReq {..} =
  sequenceA_
    [ validateField "value" value P.mobileNumber
    ]

addReferral ::
  (Id Person.Person, Id DM.Merchant) ->
  ReferralReq ->
  Flow ReferralRes
addReferral (personId, _) req = do
  runRequestValidation validateReferralReq req
  value <- encrypt req.value
  DriverInformation.addReferralCode personId value
  return Success
