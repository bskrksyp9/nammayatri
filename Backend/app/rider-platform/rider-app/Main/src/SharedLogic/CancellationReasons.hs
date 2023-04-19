module SharedLogic.CancellationReasons where

import Beckn.Types.Core.Taxi.CancellationReasons.Types
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import Storage.CachedQueries.CacheConfig

getCancellationReasons ::
  ( HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HasCacheConfig r,
    Hedis.HedisFlow m r,
    CoreMetrics m,
    HasBapInfo r m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  m CancellationReasons
getCancellationReasons providerUrl bppId city transactionId =
  Hedis.safeGet (makeCancellationReasonsId bppId) >>= \case
    Just a -> return a
    Nothing -> findAndCache providerUrl bppId city transactionId

makeCancellationReasonsId :: Text -> Text
makeCancellationReasonsId bppId = "CancellationReasons:BppId:" <> bppId

cacheCancellationReasons :: (CacheFlow m r) => Text -> CancellationReasons -> m ()
cacheCancellationReasons bppId reasons = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeCancellationReasonsId bppId) reasons expTime

findAndCache ::
  ( HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HasCacheConfig r,
    Hedis.HedisFlow m r,
    CoreMetrics m,
    HasBapInfo r m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  m CancellationReasons
findAndCache providerUrl bppId city transactionId = do
  req <- buildCancellationReasonsReq providerUrl bppId city transactionId
  reasons <- CallBPP.cancellationReasons providerUrl req
  cacheCancellationReasons bppId reasons
  return reasons

buildCancellationReasonsReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  m CancellationReasonsReq
buildCancellationReasonsReq bppUrl bppId city transactionId = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  messageId <- generateGUID
  context <- buildTaxiContext Context.GET_CANCELLATION_REASONS messageId (Just transactionId) bapIDs.cabs bapURIs.cabs (Just bppId) (Just bppUrl) city
  pure $ BecknReq context Empty

buildCancellationReasonsRes ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  CancellationReasons ->
  m CancellationReasonsRes
buildCancellationReasonsRes bppUrl bppId city transactionId listOfReasons = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  messageId <- generateGUID
  context <- buildTaxiContext Context.CANCELLATION_REASONS messageId (Just transactionId) bapIDs.cabs bapURIs.cabs (Just bppId) (Just bppUrl) city
  let message = makeCancellationReasonsMessage listOfReasons
  pure $ BecknReq context message
