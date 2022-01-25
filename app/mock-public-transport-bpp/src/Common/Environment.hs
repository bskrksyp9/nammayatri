module Common.Environment where

import Beckn.Utils.Dhall (FromDhall)
import qualified Database.Redis as Redis
import Relude
import Servant.Client

data AppCfg = AppCfg
  { port :: Int,
    selfId :: Text,
    uniqueKeyId :: Text,
    selfUri :: BaseUrl,
    redisPrefix :: Text
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { selfId :: Text,
    uniqueKeyId :: Text,
    selfUri :: BaseUrl,
    redisConnection :: Redis.Connection,
    redisPrefix :: Text
  }
  deriving (Generic)

buildAppEnv :: Redis.Connection -> AppCfg -> AppEnv
buildAppEnv redisConnection AppCfg {..} = AppEnv {..}
