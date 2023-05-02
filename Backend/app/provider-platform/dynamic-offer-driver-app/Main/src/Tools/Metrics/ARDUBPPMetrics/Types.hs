{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.ARDUBPPMetrics.Types
  ( HasBPPMetrics,
    BPPMetricsContainer (..),
    module CoreMetrics,
    RequestDurationMetric,
    registerBPPMetricsContainer,
  )
where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics as CoreMetrics
import Kernel.Utils.Common
import Prometheus as P

type HasBPPMetrics m r = (HasFlowEnv m r ["bppMetrics" ::: BPPMetricsContainer, "version" ::: DeploymentVersion])

type RequestDurationMetric = (P.Vector P.Label2 P.Histogram, P.Vector P.Label2 P.Counter)

data BPPMetricsContainer = BPPMetricsContainer
  { searchDurationTimeout :: Seconds,
    searchDuration :: RequestDurationMetric,
    selectDurationTimeout :: Seconds,
    selectDuration :: RequestDurationMetric,
    countingDeviation :: CountingDeviationMetric
  }

data CountingDeviationMetric = CountingDeviationMetric
  { realFareDeviation :: P.Vector P.Label2 P.Histogram,
    realDistanceDeviation :: P.Vector P.Label2 P.Histogram
  }

registerBPPMetricsContainer :: Seconds -> Seconds -> IO BPPMetricsContainer
registerBPPMetricsContainer searchDurationTimeout selectDurationTimeout = do
  searchDuration <- registerRequestDurationMetric "search" searchDurationTimeout
  selectDuration <- registerRequestDurationMetric "select" selectDurationTimeout
  countingDeviation <- registerCountingDeviationMetric
  return $ BPPMetricsContainer {..}

registerCountingDeviationMetric :: IO CountingDeviationMetric
registerCountingDeviationMetric =
  CountingDeviationMetric
    <$> (P.register . P.vector ("agency_name", "version") $ P.histogram fareDeviation $ aroundZero 10 5)
    <*> (P.register . P.vector ("agency_name", "version") $ P.histogram distanceDeviation $ aroundZero 10 6)
  where
    aroundZero factor b =
      let l = P.exponentialBuckets 1 factor b
       in reverse (map negate l) ++ l
    fareDeviation =
      P.Info
        "BPP_fare_deviation"
        "Difference between initially offered and recalculated fare of a ride"
    distanceDeviation =
      P.Info
        "BPP_distance_deviation"
        "Difference between estimated distance and real distance of a ride"

registerRequestDurationMetric :: Text -> Seconds -> IO RequestDurationMetric
registerRequestDurationMetric requestName requestDurationTimeout = do
  requestDurationHistogram <-
    P.register $
      P.vector ("agency_name", "version") $
        P.histogram
          infoRequestDuration
          buckets
  failureCounter <-
    P.register $
      P.vector ("agency_name", "version") $
        P.counter $ P.Info ("BPP_" <> requestName <> "_failure_counter") ""

  pure (requestDurationHistogram, failureCounter)
  where
    infoRequestDuration =
      P.Info
        ("BPP_" <> requestName <> "_time")
        ""
    buckets =
      P.linearBuckets
        0
        0.5
        requestDurationBucketCount
    requestDurationBucketCount = (getSeconds requestDurationTimeout + 1) * 2
