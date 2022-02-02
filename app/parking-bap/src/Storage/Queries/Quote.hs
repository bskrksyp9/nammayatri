module Storage.Queries.Quote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Quote
import Domain.Search
import Storage.Tabular.Quote

findById :: EsqDBFlow m r => Id Quote -> m (Maybe Quote)
findById = Esq.findById

create :: Quote -> SqlDB ()
create = create'

findAllBySearchId :: EsqDBFlow m r => Id Search -> m [Quote]
findAllBySearchId searchId =
  runTransaction . findAll' $ do
    quote <- from $ table @QuoteT
    where_ $ quote ^. QuoteSearchId ==. val (toKey searchId)
    return quote