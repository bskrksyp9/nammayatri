{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.FMD.API.Select where

import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Context
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
    message :: SelectOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype SelectOrder = SelectOrder
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type SelectRes = AckResponse

type OnSelectReq = CallbackReq SelectOrder

type OnSelectRes = AckResponse
