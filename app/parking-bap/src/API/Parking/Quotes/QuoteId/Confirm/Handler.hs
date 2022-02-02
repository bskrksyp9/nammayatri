module API.Parking.Quotes.QuoteId.Confirm.Handler where

import API.Parking.Quotes.QuoteId.Confirm.Types
import App.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Core.Common.Billing as Billing
import qualified Core.Common.Context as Context
import qualified Core.Common.Time as Time
import qualified Core.Common.Vehicle as Vehicle
import qualified Core.Confirm as Confirm
import qualified Domain.Booking as DBooking
import qualified Domain.Quote as DQuote
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Search as QSearch
import Tools.Auth
import Tools.Context (buildContext)
import Tools.Error

handler :: Id DQuote.Quote -> FlowServer API
handler = confirm

confirm :: Id DQuote.Quote -> PersonId -> PostQuoteConfirmReq -> FlowHandler PostQuoteConfirmRes
confirm quoteId _ req = withFlowHandlerAPI $ do
  quote <- QQuote.findById quoteId >>= fromMaybeM QuoteDoesNotExist
  search <- QSearch.findById quote.searchId >>= fromMaybeM SearchNotFound
  booking <- buildBooking search quote
  _ <- Esq.runTransaction $ QBooking.create booking
  let txnId = booking.id.getId
  bapURI <- asks (.config.selfURI)
  context <- buildContext Context.CONFIRM txnId bapURI (Just booking.bppUrl)
  ExternalAPI.confirm booking.bppUrl (BecknReq context $ mkConfirmMessage booking)
  return $ PostQuoteConfirmRes booking.id
  where
    buildBooking search quote = do
      uid <- generateGUID
      now <- getCurrentTime
      return $
        DBooking.Booking
          { id = Id uid,
            searchId = search.id,
            quoteId = quote.id,
            requestorId = search.requestorId,
            requestorNumber = req.requestorNumber,
            vehicleNumber = req.vehicleNumber,
            bppId = quote.bppId,
            bppUrl = quote.bppUrl,
            bppItemId = quote.bppItemId,
            parkingSpaceName = quote.parkingSpaceName,
            parkingSpaceLocationId = getId quote.parkingLocationId,
            fare = quote.fare,
            fromDate = search.fromDate,
            toDate = search.toDate,
            status = DBooking.NEW,
            ticketId = Nothing,
            ticketCreatedAt = Nothing,
            updatedAt = now,
            createdAt = now,
            bppOrderId = Nothing,
            requestorName = req.requestorName
          }
    mkConfirmMessage booking = do
      let vehicle = Vehicle.Vehicle booking.vehicleNumber
          end = Confirm.StartEnd $ Time.Time booking.toDate
          start = Confirm.StartEnd $ Time.Time booking.fromDate
          fulfillment = Confirm.Fulfillment {..}
          billing =
            Billing.Billing
              { phone = booking.requestorNumber,
                name = booking.requestorName
              }
          items = [Confirm.Item booking.bppItemId $ Confirm.Quantity 1]
          locations = [Confirm.Location booking.parkingSpaceLocationId]
          provider = Confirm.Provider {..}
          order = Confirm.Order {..}
      Confirm.ConfirmMessage {..}