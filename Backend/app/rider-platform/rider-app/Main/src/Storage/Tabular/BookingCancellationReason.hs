{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.BookingCancellationReason where

import qualified Beckn.Types.Core.Taxi.CancellationReasons.Types as SCR
import qualified Domain.Types.BookingCancellationReason as Domain
import qualified Domain.Types.CancellationReason as DCR
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Storage.Tabular.Booking as SRB
import qualified Storage.Tabular.Ride as SRide

derivePersistField "Domain.CancellationSource"

derivePersistField "DCR.CancellationStage"

derivePersistField "SCR.CancellationReasonCode"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BookingCancellationReasonT sql=booking_cancellation_reason
      bookingId SRB.BookingTId
      rideId SRide.RideTId Maybe
      source Domain.CancellationSource
      reasonCode SCR.CancellationReasonCode Maybe
      reasonStage DCR.CancellationStage Maybe
      additionalInfo Text Maybe
      Primary bookingId
      UniqueBookingCancellationReasonBookingId bookingId
      deriving Generic
    |]

instance FromTType BookingCancellationReasonT Domain.BookingCancellationReason where
  fromTType BookingCancellationReasonT {..} = do
    return $
      Domain.BookingCancellationReason
        { bookingId = fromKey bookingId,
          rideId = fromKey <$> rideId,
          ..
        }

instance ToTType BookingCancellationReasonT Domain.BookingCancellationReason where
  toTType Domain.BookingCancellationReason {..} =
    BookingCancellationReasonT
      { bookingId = toKey bookingId,
        rideId = toKey <$> rideId,
        ..
      }
