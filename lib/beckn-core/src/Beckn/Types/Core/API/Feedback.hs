module Beckn.Types.Core.API.Feedback
  ( FeedbackAPI,
    FeedbackReq (..),
    FeedbackRes,
    FeedbackReqMessage (..),
  )
where

import Beckn.Types.Core.Ack (AckResponse (..))
import qualified Beckn.Types.Core.Context as Core
import qualified Beckn.Types.Core.Description as Core
import qualified Beckn.Types.Core.Rating as Core
import Beckn.Utils.Servant.HeaderAuth (APIKeyAuth)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, type (:>))

type FeedbackAPI v =
  "feedback"
    :> APIKeyAuth v
    :> ReqBody '[JSON] FeedbackReq
    :> Post '[JSON] FeedbackRes

data FeedbackReq = FeedbackReq
  { context :: Core.Context,
    message :: FeedbackReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type FeedbackRes = AckResponse

data FeedbackReqMessage = FeedbackReqMessage
  { order_id :: Text,
    rating :: Core.Rating,
    description :: Core.Description
  }
  deriving (Generic, Show, FromJSON, ToJSON)
