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

module Storage.Tabular.DriverInformation where

import qualified Data.ByteString as BS
import qualified Domain.Types.DriverInformation as Domain
import Domain.Types.Person (Person)
import Kernel.External.Encryption (DbHash (..), Encrypted (..), EncryptedHashed (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverInformationT sql=driver_information
      driverId PersonTId
      adminId PersonTId Maybe
      merchantId MerchantTId
      active Bool
      onRide Bool
      enabled Bool
      blocked Bool
      verified Bool
      lastEnabledOn UTCTime Maybe
      referralCode Text Maybe
      canDowngradeToSedan Bool
      canDowngradeToHatchback Bool
      canDowngradeToTaxi Bool
      mode Domain.DriverMode Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary driverId
      deriving Generic
    |]

instance TEntityKey DriverInformationT where
  type DomainKey DriverInformationT = Id Person
  fromKey (DriverInformationTKey _id) = fromKey _id
  toKey id = DriverInformationTKey $ toKey id

instance FromTType DriverInformationT Domain.DriverInformation where
  fromTType DriverInformationT {..} = do
    return $
      Domain.DriverInformation
        { driverId = fromKey driverId,
          adminId = fromKey <$> adminId,
          merchantId = fromKey merchantId,
          referralCode = EncryptedHashed <$> (Encrypted <$> referralCode) <*> Just (DbHash BS.empty),
          ..
        }

instance ToTType DriverInformationT Domain.DriverInformation where
  toTType Domain.DriverInformation {..} =
    DriverInformationT
      { driverId = toKey driverId,
        adminId = toKey <$> adminId,
        merchantId = toKey merchantId,
        referralCode = referralCode <&> unEncrypted . (.encrypted),
        ..
      }
