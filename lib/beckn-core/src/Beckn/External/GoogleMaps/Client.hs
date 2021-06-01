module Beckn.External.GoogleMaps.Client where

import qualified Beckn.External.GoogleMaps.API as API
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Monitoring.Prometheus.Metrics (HasCoreMetrics)
import Beckn.Utils.Common
import EulerHS.Prelude
import GHC.Records (HasField)
import Servant.Client.Core (ClientError)

autoComplete ::
  ( HasField "dbCfg" r DBConfig,
    HasCoreMetrics r
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Integer ->
  Text ->
  FlowR r GoogleMaps.SearchLocationResp
autoComplete url apiKey input location radius components = do
  callAPI url (API.autoComplete apiKey input location radius components) "autoComplete"
    >>= fromEitherM (googleMapsError url)

placeDetails ::
  ( HasField "dbCfg" r DBConfig,
    HasCoreMetrics r
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  FlowR r GoogleMaps.PlaceDetailsResp
placeDetails url apiKey placeId fields = do
  callAPI url (API.placeDetails apiKey placeId fields) "placeDetails"
    >>= fromEitherM (googleMapsError url)

getPlaceName ::
  ( HasField "dbCfg" r DBConfig,
    HasCoreMetrics r
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  FlowR r GoogleMaps.GetPlaceNameResp
getPlaceName url latLng apiKey = do
  callAPI url (API.getPlaceName latLng apiKey) "getPlaceName"
    >>= fromEitherM (googleMapsError url)

googleMapsError :: BaseUrl -> ClientError -> ExternalAPICallError
googleMapsError = ExternalAPICallErrorWithCode "GOOGLE_MAPS_API_ERROR"
