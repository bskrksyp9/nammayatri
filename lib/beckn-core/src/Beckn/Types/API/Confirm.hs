{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Confirm where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Order
import EulerHS.Prelude

data ConfirmReq = ConfirmReq
  { context :: Context,
    message :: ConfirmOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data ConfirmRes = ConfirmRes
  { context :: Context,
    message :: Ack
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data OnConfirmReq = OnConfirmReq
  { context :: Context,
    message :: ConfirmOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype ConfirmOrder = ConfirmOrder
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data OnConfirmRes = OnConfirmRes
  { context :: Context,
    message :: Ack
  }
  deriving (Generic, Show, ToJSON, FromJSON)
