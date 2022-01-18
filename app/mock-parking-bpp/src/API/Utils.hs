module API.Utils where

import Beckn.Mock.App
import Beckn.Types.Common
import Beckn.Types.Core.Migration.Context
import Environment
import Relude

buildOnActionContext :: Action -> Context -> MockM AppEnv Context
buildOnActionContext action ctx = do
  now <- getCurrentTime
  bppId <- asks (.config.selfId)
  bppUri <- asks (.config.selfUri)
  let ctx' =
        ctx{action = action,
            bpp_id = Just bppId,
            bpp_uri = Just bppUri,
            timestamp = now
           }
  pure ctx'