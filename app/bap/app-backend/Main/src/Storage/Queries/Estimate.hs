{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Estimate where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Data.Tuple.Extra
import Domain.Types.Estimate
import Domain.Types.SearchRequest
import Storage.Queries.FullEntityBuilders (buildFullEstimate)
import Storage.Tabular.Estimate
import Storage.Tabular.TripTerms

-- order of creating entites make sense!
create :: Estimate -> SqlDB ()
create estimate =
  Esq.withFullEntity estimate $ \(estimateT, estimateBreakupT, mbTripTermsT) -> do
    traverse_ Esq.create' mbTripTermsT
    Esq.create' estimateT
    traverse_ Esq.create' estimateBreakupT

createMany :: [Estimate] -> SqlDB ()
createMany estimates =
  Esq.withFullEntities estimates $ \list -> do
    let estimateTs = map fst3 list
        estimateBreakupT = map snd3 list
        tripTermsTs = mapMaybe thd3 list
    Esq.createMany' tripTermsTs
    Esq.createMany' estimateTs
    traverse_ Esq.createMany' estimateBreakupT

fullEstimateTable ::
  From
    ( Table EstimateT
        :& Esq.MbTable TripTermsT
    )
fullEstimateTable =
  table @EstimateT
    `leftJoin` table @TripTermsT
      `Esq.on` ( \(estimate :& mbTripTerms) ->
                   estimate ^. EstimateTripTermsId ==. mbTripTerms ?. TripTermsTId
               )

findById :: Transactionable m => Id Estimate -> m (Maybe Estimate)
findById estimateId = Esq.buildDType $ do
  mbFullEstimateT <- Esq.findOne' $ do
    (estimate :& mbTripTerms) <- from fullEstimateTable
    where_ $ estimate ^. EstimateTId ==. val (toKey estimateId)
    pure (estimate, mbTripTerms)
  mapM buildFullEstimate mbFullEstimateT

findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Estimate]
findAllByRequestId searchRequestId = Esq.buildDType $ do
  fullEstimateTs <- Esq.findAll' $ do
    (estimate :& mbTripTerms) <- from fullEstimateTable
    where_ $ estimate ^. EstimateRequestId ==. val (toKey searchRequestId)
    pure (estimate, mbTripTerms)
  mapM buildFullEstimate fullEstimateTs