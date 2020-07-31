{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.FMD.API.Select where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Types.Core.Quotation
import Beckn.Types.FMD.Order
import Beckn.Utils.Servant.HeaderAuth
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type SelectAPI v =
  "select"
    :> APIKeyAuth v
    :> ReqBody '[JSON] SelectReq
    :> Post '[JSON] SelectRes

selectAPI :: Proxy (SelectAPI v)
selectAPI = Proxy

type OnSelectAPI v =
  "on_select"
    :> APIKeyAuth v
    :> ReqBody '[JSON] OnSelectReq
    :> Post '[JSON] OnSelectRes

onSelectAPI :: Proxy (OnSelectAPI v)
onSelectAPI = Proxy

data SelectReq = SelectReq
  { context :: Context,
    message :: DraftOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype DraftOrder = DraftOrder
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type SelectRes = AckResponse

data OnSelectReq = OnSelectReq
  { context :: Context,
    message :: OnSelectMessage,
    error :: Maybe Error
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OnSelectMessage = OnSelectMessage
  { order :: Order,
    quote :: Quotation
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnSelectRes = AckResponse
