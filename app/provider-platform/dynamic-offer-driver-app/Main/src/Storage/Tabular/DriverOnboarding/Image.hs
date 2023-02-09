{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverOnboarding.Image where

import qualified Domain.Types.DriverOnboarding.Error as Domain
import qualified Domain.Types.DriverOnboarding.Image as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.ImageType"
derivePersistField "Domain.DriverOnboardingError"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    ImageT sql=image
      id Text
      personId PersonTId
      merchantId MerchantTId
      s3Path Text
      imageType Domain.ImageType
      isValid Bool
      failureReason Domain.DriverOnboardingError Maybe
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey ImageT where
  type DomainKey ImageT = Id Domain.Image
  fromKey (ImageTKey _id) = Id _id
  toKey (Id id) = ImageTKey id

instance TType ImageT Domain.Image where
  fromTType ImageT {..} = do
    return $
      Domain.Image
        { id = Id id,
          merchantId = fromKey merchantId,
          personId = fromKey personId,
          ..
        }

  toTType Domain.Image {..} =
    ImageT
      { id = getId id,
        merchantId = toKey merchantId,
        personId = toKey personId,
        ..
      }