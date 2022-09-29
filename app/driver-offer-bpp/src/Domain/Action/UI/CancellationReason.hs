module Domain.Action.UI.CancellationReason
  ( list,
  )
where

import Beckn.Utils.Common
import qualified Domain.Types.CancellationReason as SCR
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.CancellationReason as QCR

list :: EsqDBFlow m r => m [SCR.CancellationReasonAPIEntity]
list = fmap SCR.makeCancellationReasonAPIEntity <$> QCR.findAll