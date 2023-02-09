module API.UI.Booking.BookingId.TriggerStatus.Handler where

import API.UI.Booking.BookingId.TriggerStatus.Types
import qualified Beckn.ACL.Status as BecknACL
import qualified Domain.Action.UI.TriggerStatus as DStatus
import qualified Domain.Types.Booking.Type as DBooking (Booking)
import Environment
import qualified ExternalAPI.Flow as ExternalAPI
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Auth

handler :: FlowServer API
handler = triggerStatusUpdate

triggerStatusUpdate :: PersonId -> Id DBooking.Booking -> FlowHandler APISuccess
triggerStatusUpdate _ bookingId = withFlowHandlerAPI $ do
  statusReq <- DStatus.triggerStatusUpdate bookingId
  becknStatusReq <- BecknACL.buildStatusReq statusReq
  ExternalAPI.status statusReq.bppUrl becknStatusReq
  pure Success