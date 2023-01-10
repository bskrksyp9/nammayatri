{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantServiceConfig
  ( findByMerchantIdAndService,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Hedis
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantServiceConfig
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Merchant.MerchantServiceConfig as Queries

findByMerchantIdAndService :: (CacheFlow m r, EsqDBFlow m r) => Id Merchant -> ServiceName -> m (Maybe MerchantServiceConfig)
findByMerchantIdAndService id serviceName =
  Hedis.get (makeMerchantIdAndServiceKey id serviceName) >>= \case
    Just a -> return . Just $ coerce @(MerchantServiceConfigD 'Unsafe) @MerchantServiceConfig a
    Nothing -> flip whenJust cacheMerchantServiceConfig /=<< Queries.findByMerchantIdAndService id serviceName

cacheMerchantServiceConfig :: CacheFlow m r => MerchantServiceConfig -> m ()
cacheMerchantServiceConfig merchantServiceConfig = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeMerchantIdAndServiceKey merchantServiceConfig.merchantId (getServiceName merchantServiceConfig)
  Hedis.setExp idKey (coerce @MerchantServiceConfig @(MerchantServiceConfigD 'Unsafe) merchantServiceConfig) expTime

makeMerchantIdAndServiceKey :: Id Merchant -> ServiceName -> Text
makeMerchantIdAndServiceKey id serviceName = "CachedQueries:MerchantServiceConfig:MerchantId-" <> id.getId <> ":ServiceName-" <> show serviceName