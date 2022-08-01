module Core.ACL.Select where

import Beckn.Prelude
import Beckn.Product.Validation.Context
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Select as Select
import qualified Beckn.Types.Core.Taxi.Select as Select
import qualified Beckn.Types.Registry.Subscriber as Subscriber
import qualified Domain.Action.Beckn.Select as DSelect
import qualified Domain.Types.SearchRequest.SearchReqLocation as Location
import Types.Error
import Utils.Common

buildSelectReq ::
  (HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text]) =>
  Subscriber.Subscriber ->
  Select.SelectReq ->
  m DSelect.DSelectReq
buildSelectReq subscriber req = do
  let context = req.context
  validateContext Context.SELECT context
  let intent = req.message.intent
  let pickup = intent.fulfillment.start
  let dropOff = fromJust intent.fulfillment.end
  unless (subscriber.subscriber_id == context.bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  let messageId = context.message_id
  pure
    DSelect.DSelectReq
      { messageId = messageId,
        transactionId = context.transaction_id,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        pickupLocation = mkLocation pickup.location,
        pickupTime = pickup.time.timestamp,
        dropLocation = mkLocation dropOff.location
      }

mkLocation :: Select.Location -> Location.SearchReqLocationAPIEntity
mkLocation (Select.Location Select.Gps {..}) =
  Location.SearchReqLocationAPIEntity
    { areaCode = Nothing,
      street = Nothing,
      door = Nothing,
      city = Nothing,
      state = Nothing,
      country = Nothing,
      building = Nothing,
      area = Nothing,
      ..
    }