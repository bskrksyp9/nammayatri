module Product.Services.GoogleMaps where

import App.Types (AppEnv (..), FlowHandler)
import qualified Beckn.External.GoogleMaps.Client as ClientGoogleMaps
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import EulerHS.Prelude
import qualified Types.Storage.RegistrationToken as RegToken
import Utils.Common (withFlowHandlerAPI)

autoComplete :: RegToken.RegistrationToken -> Text -> Text -> Integer -> Text -> FlowHandler GoogleMaps.SearchLocationResp
autoComplete _auth input location radius lang = withFlowHandlerAPI $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  let components = "country:in"
  ClientGoogleMaps.autoComplete url apiKey input location radius components lang

placeDetails :: RegToken.RegistrationToken -> Text -> FlowHandler GoogleMaps.PlaceDetailsResp
placeDetails _auth placeId = withFlowHandlerAPI $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  let fields = "geometry"
  ClientGoogleMaps.placeDetails url apiKey placeId fields

getPlaceName :: RegToken.RegistrationToken -> Text -> FlowHandler GoogleMaps.GetPlaceNameResp
getPlaceName _auth latLng = withFlowHandlerAPI $ do
  url <- googleMapsUrl <$> ask
  apiKey <- googleMapsKey <$> ask
  ClientGoogleMaps.getPlaceName url latLng apiKey
