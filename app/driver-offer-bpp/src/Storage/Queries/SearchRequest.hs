module Storage.Queries.SearchRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.SearchRequest
import Storage.Tabular.SearchReqLocation
import Storage.Tabular.SearchRequest

create :: SearchRequest -> SqlDB ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
    Esq.create' sReq
    Esq.create' fromLoc
    Esq.create' toLoc

findById :: Transactionable m => Id SearchRequest -> m (Maybe SearchRequest)
findById searchRequestId = buildDType $
  fmap (fmap extractSolidType) $
    Esq.findOne' $ do
      (sReq :& sFromLoc :& sToLoc) <-
        from
          ( table @SearchRequestT
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& loc1) -> s ^. SearchRequestFromLocationId ==. loc1 ^. SearchReqLocationTId)
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& _ :& loc2) -> s ^. SearchRequestToLocationId ==. loc2 ^. SearchReqLocationTId)
          )
      where_ $ sReq ^. SearchRequestTId ==. val (toKey searchRequestId)
      pure $ (sReq, sFromLoc, sToLoc)
