module Beckn.External.Geo.Flow where

import qualified Beckn.External.Geo.API as API
import           Beckn.External.Geo.Types
import qualified Data.Text as T
import           EulerHS.Prelude
import qualified EulerHS.Language as L
import           Servant.Client
import           System.Environment

defaultBaseUrl :: BaseUrl
defaultBaseUrl = BaseUrl Https "maps.googleapis.com" 443 ""

getLocation :: BaseUrl -> Text -> Text -> L.Flow (Maybe ReverseGeoResp)
getLocation url lat long = do
  key <- L.runIO $ T.pack <$> getEnv "GEOLOCATION_TOKEN"
  res <- L.callAPI url $ API.getLocation lat long key
  case res of
    Right v -> return $ Just v
    Left err -> L.logError "Unable to get the location: " (show err) $> Nothing

getPostalCode :: ReverseGeoResp -> Text
getPostalCode ReverseGeoResp {..} = do
  let filterAddress Address {..} =
        any (\x -> T.isInfixOf "postal" x || T.isInfixOf "pin" x) types
  maybe "NOT_FOUND" short_name (find filterAddress address_components)
