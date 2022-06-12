module Core.ACL.OnSearch where

import Beckn.Product.Validation.Context (validateContext)
import Beckn.Storage.Esqueleto (runTransaction)
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.OnSearch as OnSearch
import Beckn.Types.Id
import Beckn.Utils.Logging
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import Domain.Types.OnSearchEvent
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.VehicleVariant as VehVar
import EulerHS.Prelude hiding (id, state, unpack)
import qualified Storage.Queries.OnSearchEvent as OnSearchEvent
import Types.Error
import Utils.Common

buildOnSearchReq ::
  ( HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text],
    EsqDBFlow m r
  ) =>
  BecknCallbackReq OnSearch.OnSearchMessage ->
  m (Maybe DOnSearch.DOnSearchReq)
buildOnSearchReq req = do
  validateContext Context.ON_SEARCH $ req.context
  logOnSearchEvent req
  case req.contents of
    Right msg -> do
      let catalog = msg.catalog
      Just <$> searchCbService req.context catalog
    Left err -> do
      logTagError "on_search req" $ "on_search error: " <> show err
      pure Nothing

searchCbService :: MonadFlow m => Context.Context -> OnSearch.Catalog -> m DOnSearch.DOnSearchReq
searchCbService context catalog = do
  providerId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing bpp_id")
  providerUrl <- context.bpp_uri & fromMaybeM (InvalidRequest "Missing bpp_uri")
  -- do we need throw an error when we have more than one provider?
  case catalog.bpp_providers of
    [] -> throwError $ InvalidRequest "Missing bpp/providers" -- TODO: make it NonEmpty
    (provider : _) -> do
      let items = provider.items
      quotesInfo <- traverse buildQuoteInfo items
      let providerInfo =
            DOnSearch.ProviderInfo
              { providerId = providerId,
                name = provider.descriptor.name,
                url = providerUrl,
                mobileNumber = provider.contacts,
                ridesCompleted = provider.tags.rides_completed
              }
      pure
        DOnSearch.DOnSearchReq
          { requestId = Id context.message_id,
            ..
          }

logOnSearchEvent :: EsqDBFlow m r => OnSearch.OnSearchReq -> m ()
logOnSearchEvent (BecknCallbackReq context (leftToMaybe -> mbErr)) = do
  createdAt <- getCurrentTime
  id <- generateGUID
  bppId <- context.bpp_id & fromMaybeM (InvalidRequest "Missing context.bpp_id")
  let messageId = context.message_id
  let errorType = show . (._type) <$> mbErr
  let errorCode = (.code) <$> mbErr
  let errorMessage = (.message) =<< mbErr
  runTransaction $
    OnSearchEvent.create $ OnSearchEvent {..}

buildQuoteInfo ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m DOnSearch.QuoteInfo
buildQuoteInfo item = do
  quoteDetails <- case item.category_id of
    OnSearch.ONE_WAY_TRIP -> DQuote.OneWayAPIDetails <$> buildOneWayQuoteDetails item
    OnSearch.RENTAL_TRIP -> DQuote.RentalAPIDetails <$> buildRentalQuoteDetails item
  let itemCode = item.descriptor.code
      vehicleVariant = itemCode.vehicleVariant
      estimatedFare = realToFrac item.price.value
      estimatedTotalFare = realToFrac item.price.offered_value
      descriptions = item.quote_terms
  pure
    DOnSearch.QuoteInfo
      { discount = if estimatedTotalFare == estimatedFare then Nothing else Just $ estimatedTotalFare - estimatedFare,
        vehicleVariant = castVehicleVariant vehicleVariant,
        ..
      }
  where
    castVehicleVariant = \case
      OnSearch.SEDAN -> VehVar.SEDAN
      OnSearch.SUV -> VehVar.SUV
      OnSearch.HATCHBACK -> VehVar.HATCHBACK

buildOneWayQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m DQuote.OneWayQuoteAPIDetails
buildOneWayQuoteDetails item = do
  distanceToNearestDriver <-
    (item.tags <&> (.distance_to_nearest_driver))
      & fromMaybeM (InvalidRequest "Trip type is ONE_WAY, but distanceToNearestDriver is Nothing")
  pure
    DQuote.OneWayQuoteAPIDetails
      { distanceToNearestDriver = realToFrac distanceToNearestDriver
      }

buildRentalQuoteDetails ::
  (MonadThrow m, Log m) =>
  OnSearch.Item ->
  m DQuote.RentalQuoteAPIDetails
buildRentalQuoteDetails item = do
  baseDistance <- item.base_distance & fromMaybeM (InvalidRequest "Missing base_distance in rental search item")
  baseDuration <- item.base_duration & fromMaybeM (InvalidRequest "Missing base_duration in rental search item")
  pure DQuote.RentalQuoteAPIDetails {..}
