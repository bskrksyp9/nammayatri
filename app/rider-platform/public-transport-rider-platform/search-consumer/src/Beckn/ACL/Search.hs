module Beckn.ACL.Search where

import Beckn.Context
import qualified Beckn.Spec.Common.Context as Context
import Beckn.Spec.Common.Gps
import Beckn.Spec.Search
import qualified Domain.Action.Search as DSearch
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Id
import Kernel.Utils.Common

buildSearchReq ::
  ( MonadFlow m,
    MonadReader r m,
    HasField "bapId" r Text,
    HasField "bapURI" r BaseUrl
  ) =>
  DSearch.SearchMessage ->
  m (BecknReq SearchMessage)
buildSearchReq msg = do
  let txnId = getId (msg.searchId)
  bapId <- asks (.bapId)
  bapURI <- asks (.bapURI)
  context <- buildContext Context.SEARCH txnId bapId bapURI Nothing Nothing
  let intent = mkIntent msg
  pure (BecknReq context $ SearchMessage intent)

mkIntent :: DSearch.SearchMessage -> Intent
mkIntent msg = do
  Intent
    { fulfillment =
        Fulfillment
          { start =
              StartInfo
                { location =
                    LocationGps
                      { gps = toGps msg.gps
                      },
                  time =
                    StartTime $
                      TimeRange
                        { start = msg.fromDate,
                          end = msg.toDate
                        }
                },
            end =
              EndInfo $
                LocationGps
                  { gps = toGps msg.gps
                  }
          }
    }
  where
    toGps LatLong {..} = Gps {..}