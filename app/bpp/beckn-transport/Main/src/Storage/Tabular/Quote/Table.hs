{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote.Table where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common (HighPrecMoney)
import Beckn.Types.Id
import qualified Domain.Types.FarePolicy.FareProduct as Domain
import qualified Domain.Types.Quote as Domain
import qualified Domain.Types.Vehicle as Vehicle
import Storage.Tabular.FarePolicy.FareProduct ()
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.SearchRequest (SearchRequestTId)
import Storage.Tabular.Vehicle ()

mkPersist
  defaultSqlSettings
  [defaultQQ|
    QuoteT sql=quote
      id Text
      fareProductType Domain.FareProductType
      requestId SearchRequestTId
      estimatedFare HighPrecMoney
      discount HighPrecMoney Maybe
      estimatedTotalFare HighPrecMoney
      providerId MerchantTId
      vehicleVariant Vehicle.Variant
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey QuoteT where
  type DomainKey QuoteT = Id Domain.Quote
  fromKey (QuoteTKey _id) = Id _id
  toKey (Id id) = QuoteTKey id