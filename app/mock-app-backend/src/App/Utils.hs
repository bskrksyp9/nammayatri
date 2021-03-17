{-# LANGUAGE OverloadedLabels #-}

module App.Utils where

import App.Types
import Beckn.Types.Core.Address
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Types.Core.Location
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.FMD.Intent
import Beckn.Types.FMD.Item
import Beckn.Types.FMD.Order
import Beckn.Types.FMD.Task
import Beckn.Utils.Common
import Beckn.Utils.Example
import Control.Lens.Prism (_Just)
import Data.Default.Class
import Data.Time
import EulerHS.Prelude

address :: Address
address =
  Address
    { _name = Nothing,
      _door = "#817",
      _building = Just "Juspay Apartments",
      _street = "27th Main",
      _city = "Bangalore",
      _state = "Karnataka",
      _country = "India",
      _area_code = "560047",
      _locality = Just "8th Block Koramangala",
      _ward = Nothing
    }

address2 :: Address
address2 =
  Address
    { _name = Nothing,
      _door = "444",
      _building = Just "Fun World Amusement Park",
      _street = "",
      _city = "Bangalore",
      _state = "Karnataka",
      _country = "India",
      _area_code = "560006",
      _locality = Nothing,
      _ward = Nothing
    }

gps :: GPS
gps =
  GPS
    { lat = "12.9401108",
      lon = "77.6206631"
    }

gps2 :: GPS
gps2 =
  GPS
    { lat = "12.9401108",
      lon = "77.6306631"
    }

location :: Location
location =
  def
    { _gps = Just gps,
      _address = Just address
    }

-- In some cases (e.g. Dunzo node) it is important to have different
-- pickup and drop locations
location2 :: Location
location2 =
  def
    { _gps = Just gps2,
      _address = Just address2
    }

buildIntent :: UTCTime -> SearchIntent
buildIntent utcTime =
  SearchIntent
    { intent =
        Intent
          { _query_string = Nothing,
            _provider_id = Nothing,
            _category_id = Nothing,
            _item_id = Nothing,
            _pickups = [PickupDrop location (Just utcTime)],
            _drops = [PickupDrop location2 (Just utcTime)],
            _packages = Nothing,
            _tags = Nothing
          }
    }

buildDraftOrder :: Text -> Flow Order
buildDraftOrder itemId = do
  now <- getCurrTime
  return $
    Order
      { _id = Just "draft-task-1",
        _state = Nothing,
        _items = [example {_id = Just itemId}],
        _created_at = Just now,
        _updated_at = Just now,
        _tasks =
          [ Task
              { _id = "draft-task-1",
                _item_id = itemId,
                _next_task_id = Nothing,
                _previous_task_id = Nothing,
                _state = Nothing,
                _pickup = PickupOrDrop location Nothing example Nothing,
                _drop = PickupOrDrop location2 Nothing example Nothing,
                _return = PickupOrDrop location Nothing example Nothing,
                _package = example, -- FIXME: references item and price
                _agent = example, -- FIXME: we can't fill this
                _vehicle = example, -- FIXME: we can't fill this
                _created_at = Just now,
                _updated_at = Just now
              }
          ],
        _billing = Nothing,
        _payment = Nothing,
        _update_action = Nothing,
        _quotation = example,
        _type = Nothing,
        _prev_order_id = Nothing,
        _return_reason_id = Nothing,
        _cancellation_reasons = Nothing,
        _return_reasons = Nothing
      }

buildContext :: Text -> Text -> Flow Context
buildContext act tid = do
  utcTime <- getCurrTime
  bapNwAddr <- nwAddress <$> ask
  return $
    Context
      { _domain = FINAL_MILE_DELIVERY,
        _action = act,
        _country = Just "IND",
        _city = Nothing,
        _core_version = Just "0.8.0",
        _domain_version = Just "0.8.3",
        _bap_uri = Just bapNwAddr,
        _bpp_uri = Nothing,
        _transaction_id = tid,
        _message_id = tid,
        _timestamp = utcTime,
        _ttl = Nothing
      }

getFutureTime :: Flow UTCTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  addUTCTime 7200 <$> getCurrTime

buildSearchReq :: Text -> Flow SearchReq
buildSearchReq tid = do
  ctx <- buildContext "search" tid
  now <- getCurrTime
  let intent = buildIntent now
  pure $ SearchReq ctx intent

buildSelectReq :: Context -> Text -> Flow SelectReq
buildSelectReq ctx itemId = do
  order <- buildDraftOrder itemId
  return $
    SelectReq
      { context = ctx {_action = "select"},
        message = SelectOrder order
      }

buildInitReq :: Context -> Text -> Flow InitReq
buildInitReq ctx quotId = do
  let order = example
  return $
    InitReq
      { context = ctx {_action = "init"},
        message = InitOrder $ order & #_quotation . _Just . #_id .~ quotId
      }

buildConfirmReq :: Context -> Order -> Flow ConfirmReq
buildConfirmReq ctx order =
  return $
    ConfirmReq
      { context = ctx {_action = "confirm"},
        message = ConfirmReqMessage order
      }

updateCaller :: Context -> Flow Context
updateCaller ctx = do
  bapNwAddr <- nwAddress <$> ask
  return $ ctx {_bap_uri = Just bapNwAddr}
