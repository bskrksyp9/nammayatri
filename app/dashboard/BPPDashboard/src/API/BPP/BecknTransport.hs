module API.BPP.BecknTransport
  ( API,
    handler,
  )
where

import qualified API.BPP.BecknTransport.Driver as Driver
import "lib-dashboard" Environment
import Servant

type API =
  "beckn-transport"
    :> Driver.API

handler :: FlowServer API
handler =
  Driver.handler