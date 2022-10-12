{-# LANGUAGE AllowAmbiguousTypes #-}

module BPPClient.DriverOffer
  ( callDriverOfferBPP,
    DriversAPIs (..),
    DriverOfferAPIs (..),
  )
where

import "driver-offer-bpp" API.Dashboard as Dashboard
import Beckn.Prelude
import Beckn.Utils.Common
import qualified Dashboard.Common.Driver as Common
import Domain.Types.ServerName
import qualified EulerHS.Types as Euler
import Servant
import Tools.Client
import "lib-dashboard" Tools.Metrics

newtype DriverOfferAPIs = DriverOfferAPIs
  { drivers :: DriversAPIs
  }

data DriversAPIs = DriversAPIs
  { driverDocumentsInfo :: Euler.EulerClient Common.DriverDocumentsInfoRes,
    listDrivers :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Euler.EulerClient Common.DriverListRes,
    driverActivity :: Euler.EulerClient Common.DriverActivityRes,
    enableDrivers :: Common.DriverIds -> Euler.EulerClient Common.EnableDriversRes,
    disableDrivers :: Common.DriverIds -> Euler.EulerClient Common.DisableDriversRes,
    driverLocation :: Maybe Int -> Maybe Int -> Common.DriverIds -> Euler.EulerClient Common.DriverLocationRes
  }

mkDriverOfferAPIs :: Text -> DriverOfferAPIs
mkDriverOfferAPIs token = do
  let drivers = DriversAPIs {..}
  DriverOfferAPIs {..}
  where
    driverDocumentsInfo
      :<|> listDrivers
      :<|> driverActivity
      :<|> enableDrivers
      :<|> disableDrivers
      :<|> driverLocation = Euler.client (Proxy :: Proxy Dashboard.API) token

callDriverOfferBPP ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI DriverOfferAPIs m r b c
  ) =>
  (DriverOfferAPIs -> b) ->
  c
callDriverOfferBPP = callServerAPI @_ @m @r DRIVER_OFFER_BPP mkDriverOfferAPIs "callDriverOfferBPP"
