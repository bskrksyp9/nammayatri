{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.CancellationReason
  ( list,
    getCancellationReasons,
  )
where

import qualified Beckn.Types.Core.Taxi.CancellationReasons.Types as SCR
import qualified Domain.Types.CancellationReason as SCR
import EulerHS.Prelude hiding (id)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import qualified Storage.Queries.CancellationReason as QCR

list :: EsqDBReplicaFlow m r => m [SCR.CancellationReasonAPIEntity]
list = fmap SCR.makeCancellationReasonAPIEntity <$> getCancellationReasons

getCancellationReasons :: EsqDBReplicaFlow m r => m [SCR.CancellationReason]
getCancellationReasons = runInReplica QCR.findAll
