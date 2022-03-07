{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.Quote as Domain
import qualified Storage.Tabular.SearchRequest as SSearchRequest

mkPersist
  defaultSqlSettings
  [defaultQQ|
    QuoteT sql=quote
      id Text
      bppQuoteId Text
      requestId SSearchRequest.SearchRequestTId
      estimatedFare Amount
      discount Amount Maybe
      estimatedTotalFare Amount
      providerId Text
      providerUrl Text
      providerName Text
      providerMobileNumber Text
      providerCompletedRidesCount Int
      distanceToNearestDriver Double
      vehicleVariant Text
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey QuoteT where
  type DomainKey QuoteT = Id Domain.Quote
  fromKey (QuoteTKey _id) = Id _id
  toKey (Id id) = QuoteTKey id

instance TEntity QuoteT Domain.Quote where
  fromTEntity entity = do
    let QuoteT {..} = entityVal entity
    pUrl <- parseBaseUrl providerUrl
    return $
      Domain.Quote
        { id = Id id,
          bppQuoteId = Id bppQuoteId,
          requestId = fromKey requestId,
          providerUrl = pUrl,
          ..
        }
  toTType Domain.Quote {..} =
    QuoteT
      { id = getId id,
        bppQuoteId = getId bppQuoteId,
        requestId = toKey requestId,
        providerUrl = showBaseUrl providerUrl,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a