module Mobility.SuccessFlow where

import Beckn.Types.Id
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import qualified "beckn-transport" Storage.Queries.Ride as TQRide
import qualified "beckn-transport" Storage.Queries.RideBooking as TQRB
import qualified "beckn-transport" Types.API.RideBooking as RideBookingAPI
import qualified "app-backend" Types.Storage.Quote as BQuote
import qualified "beckn-transport" Types.Storage.Quote as TQuote
import qualified "app-backend" Types.Storage.Ride as BRide
import qualified "beckn-transport" Types.Storage.Ride as TRide
import qualified "app-backend" Types.Storage.RideBooking as AppRB
import qualified "app-backend" Types.Storage.RideBooking as BRB
import qualified "beckn-transport" Types.Storage.RideBooking as TRB
import Utils

doAnAppSearch :: HasCallStack => ClientsM (Id BQuote.Quote, Id BRB.RideBooking)
doAnAppSearch = do
  -- Driver sets online
  void . callBPP $ setDriverOnline driverToken True

  -- Do an App Search
  appSearchId <-
    callBAP $
      searchServices appRegistrationToken searchReq
        <&> (.searchId)

  -- Do a Case Status request for getting product instance to confirm ride
  quoteAPIEntity <- expectSingletonNE <=< poll $ do
    -- List all confirmed rides (type = RIDEORDER)
    callBAP (getQuotes appSearchId appRegistrationToken)
      <&> (.quotes)
      -- since all BPP can give quote for now we filter by orgId
      <&> filter (\p -> p.agencyName == bapTransporterName)
      <&> nonEmpty
  let bQuoteId = quoteAPIEntity.id

  -- check if calculated price is greater than 0
  quoteAPIEntity.estimatedPrice `shouldSatisfy` (> 100) -- should at least be more than 100

  -- Confirm ride from app backend
  confirmResult <-
    callBAP $
      appConfirmRide appRegistrationToken appSearchId bQuoteId
  let bRideBookingId = confirmResult.bookingId

  return (bQuoteId, bRideBookingId)

getBPPRideBooking ::
  Id TQuote.Quote ->
  ClientsM TRB.RideBooking
getBPPRideBooking quoteId = do
  mbRideBooking <- liftIO $ runTransporterFlow "" $ TQRB.findByQuoteId quoteId
  mbRideBooking `shouldSatisfy` isJust
  return $ fromJust mbRideBooking

getBPPRide ::
  Id TRB.RideBooking ->
  ClientsM TRide.Ride
getBPPRide rideBookingId = do
  mbRide <- liftIO $ runTransporterFlow "" $ TQRide.findByRBId rideBookingId
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for successful booking and completion of ride" $ withBecknClients clients do
      (quoteId, bRideBookingId) <- doAnAppSearch

      tRideBooking <- poll $ do
        trb <- getBPPRideBooking (cast quoteId)
        trb.status `shouldBe` TRB.CONFIRMED
        return $ Just trb

      rideInfo <-
        poll . callBPP $
          getNotificationInfo (cast tRideBooking.id) driverToken
            <&> (.rideRequest)
      rideInfo.bookingId `shouldBe` tRideBooking.id

      -- Driver Accepts a ride
      void . callBPP $
        rideRespond tRideBooking.id driverToken $
          RideBookingAPI.SetDriverAcceptanceReq RideBookingAPI.ACCEPT

      tRide <- poll $ do
        tRide <- getBPPRide tRideBooking.id
        tRide.status `shouldBe` TRide.NEW
        return $ Just tRide

      void . callBPP $
        rideStart driverToken tRide.id $
          buildStartRideReq tRide.otp

      void . poll $ do
        inprogressRBStatusResult <- callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
        inprogressRBStatusResult.ride `shouldSatisfy` isJust
        inprogressRBStatusResult.status `shouldBe` AppRB.TRIP_ASSIGNED
        let Just inprogressRide = inprogressRBStatusResult.ride
        inprogressRide.status `shouldBe` BRide.INPROGRESS
        return $ Just ()

      void . callBPP $ rideEnd driverToken tRide.id

      completedRideId <- poll $ do
        completedRBStatusResult <- callBAP (appRideBookingStatus bRideBookingId appRegistrationToken)
        completedRBStatusResult.ride `shouldSatisfy` isJust
        completedRBStatusResult.status `shouldBe` AppRB.COMPLETED
        let Just completedRide = completedRBStatusResult.ride
        completedRide.status `shouldBe` BRide.COMPLETED
        return $ Just completedRide.id

      -- Leave feedback
      void . callBAP $ callAppFeedback 5 completedRideId
