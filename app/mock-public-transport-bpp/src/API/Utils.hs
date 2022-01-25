module API.Utils where

import Beckn.Types.Core.Migration.Context
import Common.App
import Common.Environment
import Relude

buildOnActionContext :: Action -> Context -> MockM AppEnv Context
buildOnActionContext action ctx = do
  now <- getCurrentTime
  bppId <- asks (.selfId)
  bppUri <- asks (.selfUri)
  let ctx' =
        ctx{action = action,
            bpp_id = Just bppId,
            bpp_uri = Just bppUri,
            timestamp = now
           }
  pure ctx'
