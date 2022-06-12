{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RideBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RideBooking as Domain
import qualified Domain.Types.VehicleVariant as VehVar (VehicleVariant)
import qualified Storage.Tabular.BookingLocation as SLoc
import qualified Storage.Tabular.Person as SPerson
import Storage.Tabular.Quote ()
import qualified Storage.Tabular.RentalSlab as SRentalSlab
import qualified Storage.Tabular.TripTerms as STripTerms
import Types.Error
import Utils.Common hiding (id)

derivePersistField "Domain.RideBookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideBookingT sql=ride_booking
      id Text
      fareProductType DQuote.FareProductType
      bppBookingId Text Maybe sql=bpp_ride_booking_id
      status Domain.RideBookingStatus
      providerId Text
      providerUrl Text
      providerName Text
      providerMobileNumber Text
      startTime UTCTime
      riderId SPerson.PersonTId
      fromLocationId SLoc.BookingLocationTId
      toLocationId SLoc.BookingLocationTId Maybe
      estimatedFare Amount
      discount Amount Maybe
      estimatedTotalFare Amount
      distance Double Maybe
      vehicleVariant VehVar.VehicleVariant
      tripTermsId STripTerms.TripTermsTId Maybe
      rentalSlabId SRentalSlab.RentalSlabTId Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RideBookingT where
  type DomainKey RideBookingT = Id Domain.RideBooking
  fromKey (RideBookingTKey _id) = Id _id
  toKey (Id id) = RideBookingTKey id

data RideBookingDetailsT = OneWayDetailsT | RentalDetailsT SRentalSlab.RentalSlabT

type FullRideBookingT = (RideBookingT, Maybe STripTerms.TripTermsT, RideBookingDetailsT)

instance TType FullRideBookingT Domain.RideBooking where
  fromTType (RideBookingT {..}, mbTripTermsT, rideBookingDetailsT) = do
    pUrl <- parseBaseUrl providerUrl
    tripTerms <- forM mbTripTermsT fromTType
    rideBookingDetails <- case rideBookingDetailsT of
      OneWayDetailsT -> do
        toLocationId' <- toLocationId & fromMaybeM (InternalError "toLocationId is null for one way ride booking")
        distance' <- distance & fromMaybeM (InternalError "distance is null for one way ride booking")
        pure . Domain.OneWayDetails $
          Domain.OneWayRideBookingDetails
            { toLocationId = fromKey toLocationId',
              distance = HighPrecMeters distance'
            }
      RentalDetailsT rentalSlabT ->
        pure . Domain.RentalDetails $ fromRentalSlabTType rentalSlabT
    return $
      Domain.RideBooking
        { id = Id id,
          bppBookingId = Id <$> bppBookingId,
          riderId = fromKey riderId,
          fromLocationId = fromKey fromLocationId,
          providerUrl = pUrl,
          ..
        }
  toTType Domain.RideBooking {..} = do
    let (fareProductType, rideBookingDetailsT, toLocationId, distance, rentalSlabId) = case rideBookingDetails of
          Domain.OneWayDetails details -> (DQuote.ONE_WAY, OneWayDetailsT, Just . toKey $ details.toLocationId, Just details.distance, Nothing)
          Domain.RentalDetails details -> do
            let rentalSlabT = toRentalSlabTType details
            (DQuote.RENTAL, RentalDetailsT rentalSlabT, Nothing, Nothing, Just . toKey $ details.slabId)

        rideBookingT =
          RideBookingT
            { id = getId id,
              bppBookingId = getId <$> bppBookingId,
              riderId = toKey riderId,
              fromLocationId = toKey fromLocationId,
              providerUrl = showBaseUrl providerUrl,
              tripTermsId = toKey <$> (tripTerms <&> (.id)),
              distance = getHighPrecMeters <$> distance,
              ..
            }
    let mbTripTermsT = toTType <$> tripTerms
    (rideBookingT, mbTripTermsT, rideBookingDetailsT)

-- use instance instead
fromRentalSlabTType :: SRentalSlab.RentalSlabT -> Domain.RentalRideBookingDetails
fromRentalSlabTType SRentalSlab.RentalSlabT {..} =
  Domain.RentalRideBookingDetails
    { slabId = Id id,
      baseDistance = Kilometers baseDistance,
      baseDuration = Hours baseDuration
    }

toRentalSlabTType :: Domain.RentalRideBookingDetails -> SRentalSlab.RentalSlabT
toRentalSlabTType Domain.RentalRideBookingDetails {..} =
  SRentalSlab.RentalSlabT
    { id = getId slabId,
      baseDistance = getKilometers baseDistance,
      baseDuration = getHours baseDuration
    }
