module Mobility.ARDU.Utils (module Mobility.ARDU.Utils) where

import qualified "driver-offer-bpp" API.UI.Driver as TDriver
import qualified "driver-offer-bpp" API.UI.Ride as RideAPI
import qualified "app-backend" API.UI.Search as AppSearch
import qualified Beckn.External.Maps as Maps
import Beckn.External.Maps.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Common (Money)
import Beckn.Types.Id
import Common
import qualified "app-backend" Domain.Action.UI.Cancel as AppCancel
import qualified "app-backend" Domain.Types.Booking as AppRB
import qualified "driver-offer-bpp" Domain.Types.Booking as TRB
import qualified "app-backend" Domain.Types.CancellationReason as AppCR
import qualified "driver-offer-bpp" Domain.Types.CancellationReason as SCR
import qualified "driver-offer-bpp" Domain.Types.DriverInformation as TDrInfo
import qualified "app-backend" Domain.Types.Estimate as AppEstimate
import qualified "driver-offer-bpp" Domain.Types.Merchant as TDM
import qualified "driver-offer-bpp" Domain.Types.Merchant.MerchantServiceConfig as TDMSC
import "driver-offer-bpp" Domain.Types.Person as TPerson
import qualified "app-backend" Domain.Types.Quote as AppQuote
import qualified "app-backend" Domain.Types.Ride as BRide
import qualified "driver-offer-bpp" Domain.Types.Ride as TRide
import qualified "app-backend" Domain.Types.SearchRequest as AppSearchReq
import qualified "driver-offer-bpp" Domain.Types.SearchRequest as ArduSReq
import Domain.Types.SearchRequestForDriver as SearchReqInfo
import HSpec
import qualified Mobility.ARDU.APICalls as API
import Mobility.ARDU.Fixtures as Fixtures
import Mobility.ARDU.Queries as Queries
import Mobility.AppBackend.APICalls as BapAPI
import Mobility.AppBackend.Fixtures
import Servant.Client
import qualified "driver-offer-bpp" Storage.CachedQueries.Merchant.MerchantServiceConfig as TCQMSC
import qualified "app-backend" Storage.Queries.Booking as BQRB
import qualified "driver-offer-bpp" Storage.Queries.Booking as TQRB
import qualified "driver-offer-bpp" Storage.Queries.DriverInformation as QTDrInfo
import "driver-offer-bpp" Storage.Queries.DriverLocation
import qualified Storage.Queries.DriverQuote as TDQ
import qualified "app-backend" Storage.Queries.Ride as BQRide
import qualified "driver-offer-bpp" Storage.Queries.Ride as TQRide
import qualified "driver-offer-bpp" Storage.Queries.SearchRequest as QSReq
import Utils

-- database calls
getBAPBooking ::
  Id AppRB.Booking ->
  ClientsM AppRB.Booking
getBAPBooking bapRBId = do
  mbBRB <- liftIO $ runAppFlow "" $ BQRB.findById bapRBId
  mbBRB `shouldSatisfy` isJust
  let Just bRB = mbBRB
  return bRB

getBPPBooking ::
  Id AppRB.Booking ->
  ClientsM TRB.Booking
getBPPBooking bapRBId = do
  bRB <- getBAPBooking bapRBId
  bRB.bppBookingId `shouldSatisfy` isJust
  let Just bppBookingId = bRB.bppBookingId
  mbTRB <- liftIO $ runARDUFlow "" $ TQRB.findById $ cast bppBookingId
  mbTRB $> () `shouldSatisfy` isJust
  let Just tRB = mbTRB
  return tRB

getBPPRide ::
  Id TRB.Booking ->
  ClientsM TRide.Ride
getBPPRide rideBookingId = do
  mbRide <- liftIO $ runARDUFlow "" $ TQRide.findActiveByRBId rideBookingId
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

getBAPRide ::
  Id TRide.Ride ->
  ClientsM BRide.Ride
getBAPRide bppRideId = do
  mbRide <- liftIO $ runAppFlow "" $ BQRide.findByBPPRideId (cast bppRideId)
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

getBPPRideById ::
  Id TRide.Ride ->
  ClientsM TRide.Ride
getBPPRideById rideId = do
  mbRide <- liftIO $ runARDUFlow "" $ TQRide.findById rideId
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

getBPPDriverLocation ::
  Id TPerson.Person ->
  ClientsM LatLong
getBPPDriverLocation driverId = do
  mbRes <- liftIO $ runARDUFlow "" $ findById driverId
  mbRes `shouldSatisfy` isJust
  let res = fromJust mbRes
  pure $
    LatLong
      { lat = res.lat,
        lon = res.lon
      }

getBPPDriverInformation ::
  Id TPerson.Person ->
  ClientsM TDrInfo.DriverInformation
getBPPDriverInformation driverId =
  liftIO $ poll $ runARDUFlow "" $ QTDrInfo.findById $ cast driverId

-- driver setup/reset
setupDriver :: DriverTestData -> LatLong -> ClientsM ()
setupDriver driver initialPoint = do
  void . callBPP $ API.ui.driver.setDriverOnline driver.token True
  -- Moves driver to the pickup point
  preUpdate <- liftIO $ API.buildUpdateLocationRequest $ initialPoint :| []
  void . callBPP $
    API.ui.location.updateLocation driver.token preUpdate

resetDriver :: DriverTestData -> IO ()
resetDriver driver = runARDUFlow "" $ do
  rides <- TQRide.findAllByDriverId (cast driver.driverId) Nothing Nothing (Just True) Nothing
  activeQuotes <- TDQ.findActiveQuotesByDriverId (cast driver.driverId) 99999
  Esq.runTransaction $ do
    void . forM rides $ \(ride, booking) -> do
      TQRide.updateStatus ride.id TRide.CANCELLED
      TQRB.updateStatus booking.id TRB.CANCELLED
    void . forM activeQuotes $ \activeQuote ->
      TDQ.setInactiveByRequestId activeQuote.searchRequestId
    QTDrInfo.updateActivity (cast driver.driverId) False
    QTDrInfo.updateOnRide (cast driver.driverId) False

-- flow primitives
search :: Text -> AppSearch.SearchReq -> ClientsM (Id AppSearchReq.SearchRequest)
search token searchReq_ = callBAP $ searchServices token searchReq_ (Just defaultVersion) (Just defaultVersion) <&> (.searchId)

getOnSearchTaxiEstimatesByTransporterName ::
  Text ->
  Id AppSearchReq.SearchRequest ->
  Text ->
  ClientsM (NonEmpty AppEstimate.EstimateAPIEntity)
getOnSearchTaxiEstimatesByTransporterName appToken searchId transporterName =
  pollFilteredList
    "get on_search estimates"
    (\p -> p.agencyName == transporterName)
    $ callBAP (getQuotes searchId appToken)
      <&> (.estimates)

select :: Text -> Id AppEstimate.Estimate -> ClientsM ()
select bapToken quoteId = void $ callBAP $ selectQuote bapToken quoteId

getNearbySearchRequestForDriver :: DriverTestData -> Id AppEstimate.Estimate -> ClientsM (NonEmpty SearchRequestForDriverAPIEntity)
getNearbySearchRequestForDriver driver estimateId =
  pollFilteredMList
    "get at least one nearby search request for driver"
    ( \p -> do
        mbSReq <- liftIO $ runARDUFlow "" $ QSReq.findById p.searchRequestId
        pure $ fmap (.messageId) mbSReq == Just estimateId.getId
    )
    ((.searchRequestsForDriver) <$> callBPP (API.ui.driver.getNearbySearchRequests driver.token))

respondQuote :: DriverTestData -> Money -> Id ArduSReq.SearchRequest -> SearchReqInfo.SearchRequestForDriverResponse -> ClientsM ()
respondQuote driver fare bppSearchRequestId response =
  void $ callBPP $ API.ui.driver.respondQuote driver.token $ TDriver.DriverRespondReq (Just fare) bppSearchRequestId response

offerQuote :: DriverTestData -> Money -> Id ArduSReq.SearchRequest -> ClientsM ()
offerQuote driver fare bppSearchRequestId =
  void $ callBPP $ API.ui.driver.offerQuote driver.token $ TDriver.DriverOfferReq (Just fare) bppSearchRequestId

respondQuoteEither :: DriverTestData -> Money -> Id ArduSReq.SearchRequest -> SearchReqInfo.SearchRequestForDriverResponse -> ClientsM (Either ClientError APISuccess)
respondQuoteEither driver fare bppSearchRequestId response =
  callBppEither $ API.ui.driver.respondQuote driver.token $ TDriver.DriverRespondReq (Just fare) bppSearchRequestId response

offerQuoteEither :: DriverTestData -> Money -> Id ArduSReq.SearchRequest -> ClientsM (Either ClientError APISuccess)
offerQuoteEither driver fare bppSearchRequestId =
  callBppEither $ API.ui.driver.offerQuote driver.token $ TDriver.DriverOfferReq (Just fare) bppSearchRequestId

getQuotesByEstimateId :: Text -> Id AppEstimate.Estimate -> ClientsM (NonEmpty AppQuote.QuoteAPIEntity)
getQuotesByEstimateId appToken estimateId =
  pollFilteredList
    "get at least one on_select quote"
    (\p -> p.agencyName == bapTransporterName)
    ((.selectedQuotes) <$> callBAP (selectList appToken estimateId))

confirmWithCheck :: Text -> Id AppQuote.Quote -> ClientsM (Id AppRB.Booking, TRB.Booking, TRide.Ride)
confirmWithCheck appToken quoteId = do
  bBookingId <- fmap (.bookingId) $ callBAP $ BapAPI.appConfirmRide appToken quoteId

  void . pollDesc "booking exists" $ do
    initRB <- getBAPBooking bBookingId
    initRB.bppBookingId `shouldSatisfy` isJust
    return $ Just ()

  void . pollDesc "ride confirmed and assigned" $
    callBAP (appBookingStatus bBookingId appToken)
      <&> (.status)
      >>= (`shouldBe` AppRB.TRIP_ASSIGNED)
      <&> Just

  tBooking <- pollDesc "trip assigned" $ do
    trb <- getBPPBooking bBookingId
    trb.status `shouldBe` TRB.TRIP_ASSIGNED
    return $ Just trb

  tRide <- pollDesc "new ride" $ do
    tRide <- getBPPRide tBooking.id
    tRide.status `shouldBe` TRide.NEW
    return $ Just tRide

  pure (bBookingId, tBooking, tRide)

startRide :: DriverTestData -> LatLong -> TRide.Ride -> Id AppRB.Booking -> ClientsM ()
startRide driver origin tRide bBookingId = do
  void . callBPP $
    API.ui.ride.rideStart driver.token tRide.id $
      API.buildStartRideReq tRide.otp origin

  void . pollDesc "trip started" $ do
    inprogressRBStatusResult <- callBAP (appBookingStatus bBookingId appRegistrationToken)
    inprogressRBStatusResult.rideList `shouldSatisfy` not . null
    inprogressRBStatusResult.status `shouldBe` AppRB.TRIP_ASSIGNED
    let [inprogressRide] = inprogressRBStatusResult.rideList
    inprogressRide.status `shouldBe` BRide.INPROGRESS
    return $ Just ()

endRide ::
  DriverTestData ->
  LatLong ->
  TRide.Ride ->
  Id AppRB.Booking ->
  ClientsM ()
endRide driver destination tRide bBookingId = do
  void . callBPP $ API.ui.ride.rideEnd driver.token tRide.id $ RideAPI.EndRideReq destination
  void $
    pollDesc "ride completed" $ do
      completedRBStatusResult <- callBAP (appBookingStatus bBookingId appRegistrationToken)
      completedRBStatusResult.rideList `shouldSatisfy` not . null
      completedRBStatusResult.status `shouldBe` AppRB.COMPLETED
      let [completedRide] = completedRBStatusResult.rideList
      completedRide.status `shouldBe` BRide.COMPLETED
      return $ Just completedRide.id

cancellationChecks :: Id AppRB.Booking -> DriverTestData -> ClientsM ()
cancellationChecks bapBookingId driver =
  void $
    pollDesc "ride cancelled by driver" $ do
      bapBooking <- getBAPBooking bapBookingId
      bapBooking.status `shouldBe` AppRB.CANCELLED
      bppBooking <- getBPPBooking bapBookingId
      bppBooking.status `shouldBe` TRB.CANCELLED
      driverInfo <- getBPPDriverInformation $ cast driver.driverId
      driverInfo.onRide `shouldBe` False

      pure $ Just ()

cancelRideByDriver :: DriverTestData -> Id AppRB.Booking -> TRide.Ride -> ClientsM ()
cancelRideByDriver driver bapBookingId tRide = do
  void . callBPP $
    API.ui.ride.rideCancel driver.token tRide.id $
      RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing
  cancellationChecks bapBookingId driver

cancelRideByApp :: Text -> DriverTestData -> Id AppRB.Booking -> ClientsM ()
cancelRideByApp appToken driver bapBookingId = do
  void . callBAP $
    BapAPI.cancelRide bapBookingId appToken $
      AppCancel.CancelReq
        { reasonCode = AppCR.CancellationReasonCode "",
          reasonStage = AppCR.OnAssign,
          additionalInfo = Nothing
        }
  cancellationChecks bapBookingId driver

badCancelRideByDriver :: DriverTestData -> SearchConfirmResult -> ClientsM ()
badCancelRideByDriver driver SearchConfirmResult {..} = do
  withFakeBapUrl bppBooking $
    void . callBPP $
      API.ui.ride.rideCancel driver.token ride.id $
        RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing
  badCancellationChecks bapBookingId driver

badCancellationChecks :: Id AppRB.Booking -> DriverTestData -> ClientsM ()
badCancellationChecks bapBookingId driver =
  void $
    pollDesc "ride cancelled by driver" $ do
      bapBooking <- getBAPBooking bapBookingId
      bapBooking.status `shouldBe` AppRB.TRIP_ASSIGNED
      bppBooking <- getBPPBooking bapBookingId
      bppBooking.status `shouldBe` TRB.CANCELLED
      driverInfo <- getBPPDriverInformation $ cast driver.driverId
      driverInfo.onRide `shouldBe` False

      pure $ Just ()

-- aggregate functions

-- from search to select (inclusive)
search'Select :: Text -> AppSearch.SearchReq -> ClientsM (Id AppEstimate.Estimate)
search'Select appToken searchReq' = do
  appSearchId <- search appToken searchReq'
  (bapQuoteAPIEntity :| _) <- getOnSearchTaxiEstimatesByTransporterName appToken appSearchId bapTransporterName
  let quoteId = bapQuoteAPIEntity.id
  select appToken quoteId
  pure quoteId

data SearchConfirmResult = SearchConfirmResult
  { bapBookingId :: Id AppRB.Booking,
    bppBooking :: TRB.Booking,
    ride :: TRide.Ride
  }

search'Confirm :: Text -> DriverTestData -> AppSearch.SearchReq -> ClientsM SearchConfirmResult
search'Confirm appToken driver searchReq' = do
  estimateId <- search'Select appToken searchReq'

  (searchReqForDriver :| _) <- getNearbySearchRequestForDriver driver estimateId

  offerQuote driver defaultAllowedDriverFee searchReqForDriver.searchRequestId

  (quoteAPIEntity :| _) <- getQuotesByEstimateId appToken estimateId
  let quoteId = quoteAPIEntity.id

  (bapBookingId, bppBooking, ride) <- confirmWithCheck appToken quoteId
  pure $
    SearchConfirmResult
      { bapBookingId,
        bppBooking,
        ride
      }

changeCachedMapsConfig :: Maps.MapsServiceConfig -> IO ()
changeCachedMapsConfig googleCfg = runARDUFlow "change cached maps config" $ do
  let serviceConfig = TDMSC.MapsServiceConfig googleCfg
  nammaYatriPartnerServiceConfig <- TDMSC.buildMerchantServiceConfig Fixtures.nammaYatriPartnerMerchantId serviceConfig
  otherMerchant2ServiceConfig <- TDMSC.buildMerchantServiceConfig Fixtures.otherMerchant2Id serviceConfig
  TCQMSC.cacheMerchantServiceConfig nammaYatriPartnerServiceConfig
  TCQMSC.cacheMerchantServiceConfig otherMerchant2ServiceConfig

clearCachedMapsConfig :: IO ()
clearCachedMapsConfig = runARDUFlow "clear cached maps config" do
  TCQMSC.clearCache Fixtures.nammaYatriPartnerMerchantId (TDMSC.MapsService Maps.Google)
  TCQMSC.clearCache Fixtures.otherMerchant2Id (TDMSC.MapsService Maps.Google)

rideSync :: ShortId TDM.Merchant -> Id TRide.Ride -> ClientsM ()
rideSync merchantId rideId = do
  let dashboardAPI = API.dashboard merchantId Fixtures.dashboardToken
  void . callBPP $ dashboardAPI.ride.rideSync (cast rideId)

withFakeBapUrl :: TRB.Booking -> ClientsM () -> ClientsM ()
withFakeBapUrl booking action = do
  liftIO $
    runARDUFlow "fake bap url" $ do
      Esq.runTransaction $
        Queries.updateBapUrlWithFake booking.id
  action
  liftIO $
    runARDUFlow "update bap url" $ do
      Esq.runTransaction $
        Queries.updateBapUrl booking.bapUri booking.id
