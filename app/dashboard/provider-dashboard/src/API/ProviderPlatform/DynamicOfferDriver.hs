module API.ProviderPlatform.DynamicOfferDriver
  ( API,
    handler,
  )
where

import qualified API.ProviderPlatform.DynamicOfferDriver.Booking as Booking
import qualified API.ProviderPlatform.DynamicOfferDriver.Driver as Driver
import qualified API.ProviderPlatform.DynamicOfferDriver.Merchant as Merchant
import qualified API.ProviderPlatform.DynamicOfferDriver.Message as Message
import qualified API.ProviderPlatform.DynamicOfferDriver.Ride as Ride
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Kernel.Types.Id
import Servant

type API =
  "driver-offer"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> ( Driver.API
           :<|> Ride.API
           :<|> Booking.API
           :<|> Merchant.API
           :<|> Message.API
       )

handler :: FlowServer API
handler merchantId =
  Driver.handler merchantId
    :<|> Ride.handler merchantId
    :<|> Booking.handler merchantId
    :<|> Merchant.handler merchantId
    :<|> Message.handler merchantId