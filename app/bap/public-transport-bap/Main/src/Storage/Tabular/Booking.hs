{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Booking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common (HighPrecMoney)
import Beckn.Types.Id
import qualified Domain.Types.Booking as Domain
import Storage.Tabular.Quote (QuoteTId)
import Storage.Tabular.Search (SearchTId)
import Storage.Tabular.TransportStation (TransportStationTId)

derivePersistField "Domain.BookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BookingT sql=booking
      id Text
      searchId SearchTId
      quoteId QuoteTId
      bknTxnId Text
      requestorId Text
      quantity Int
      bppId Text
      bppUrl Text
      publicTransportSupportNumber Text
      description Text
      fare HighPrecMoney
      departureTime UTCTime
      arrivalTime UTCTime
      departureStationId TransportStationTId
      arrivalStationId TransportStationTId
      status Domain.BookingStatus
      ticketId Text Maybe
      ticketCreatedAt UTCTime Maybe
      updatedAt UTCTime
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey BookingT where
  type DomainKey BookingT = Id Domain.Booking
  fromKey (BookingTKey _id) = Id _id
  toKey id = BookingTKey id.getId

instance TType BookingT Domain.Booking where
  fromTType BookingT {..} = do
    bppUrl_ <- parseBaseUrl bppUrl
    return $
      Domain.Booking
        { id = Id id,
          searchId = fromKey searchId,
          quoteId = fromKey quoteId,
          requestorId = Id requestorId,
          bppUrl = bppUrl_,
          departureStationId = fromKey departureStationId,
          arrivalStationId = fromKey arrivalStationId,
          fare = roundToIntegral fare,
          ..
        }
  toTType Domain.Booking {..} = do
    BookingT
      { id = id.getId,
        searchId = toKey searchId,
        quoteId = toKey quoteId,
        requestorId = requestorId.getId,
        bppUrl = showBaseUrl bppUrl,
        departureStationId = toKey departureStationId,
        arrivalStationId = toKey arrivalStationId,
        fare = realToFrac fare,
        ..
      }