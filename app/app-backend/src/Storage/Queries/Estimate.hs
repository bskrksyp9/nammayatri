{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Estimate where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq hiding (Table)
import Beckn.Types.Id
import Domain.Types.Estimate
import Domain.Types.SearchRequest
import Storage.Tabular.Estimate
import Storage.Tabular.TripTerms

-- order of creating entites make sense!
create :: Estimate -> SqlDB ()
create estimate =
  Esq.withFullEntity estimate $ \(estimateT, mbTripTermsT) -> do
    traverse_ Esq.create' mbTripTermsT
    Esq.create' estimateT

createMany :: [Estimate] -> SqlDB ()
createMany estimates =
  Esq.withFullEntities estimates $ \list -> do
    let estimateTs = map fst list
        tripTermsTs = mapMaybe snd list
    Esq.createMany' tripTermsTs
    Esq.createMany' estimateTs

-- remove when https://bitbucket.org/juspay/beckn-shared-kernel/pull-requests/35 will be merged
type Table a = SqlExpr (Entity a)

type MbTable a = SqlExpr (Maybe (Entity a))

fullEstimateTable ::
  From
    ( Table EstimateT
        :& MbTable TripTermsT
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
  pure $ extractSolidType <$> mbFullEstimateT

findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Estimate]
findAllByRequestId searchRequestId = Esq.buildDType $ do
  fullEstimateTs <- Esq.findAll' $ do
    (estimate :& mbTripTerms) <- from fullEstimateTable
    where_ $ estimate ^. EstimateRequestId ==. val (toKey searchRequestId)
    pure (estimate, mbTripTerms)
  pure $ extractSolidType <$> fullEstimateTs