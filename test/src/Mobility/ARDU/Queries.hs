module Mobility.ARDU.Queries where

import qualified "driver-offer-bpp" Domain.Types.Booking as DBooking
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Servant.Client as Servant
import "driver-offer-bpp" Storage.Tabular.Booking

updateBapUrl :: BaseUrl -> Id DBooking.Booking -> Esq.SqlDB ()
updateBapUrl bapUrl bookingId = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingBapUri =. val (showBaseUrl bapUrl)
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey bookingId)

updateBapUrlWithFake :: Id DBooking.Booking -> Esq.SqlDB ()
updateBapUrlWithFake = updateBapUrl fakeUrl

fakeUrl :: BaseUrl
fakeUrl =
  Servant.BaseUrl
    { baseUrlScheme = Servant.Http,
      baseUrlHost = "fakeUrl",
      baseUrlPort = 80,
      baseUrlPath = ""
    }