module Epass.Types.API.Blacklist where

import Data.Default
import Data.Swagger
import Data.Time.LocalTime
import Epass.Types.App
import Epass.Types.Common
import Epass.Types.Storage.Blacklist
import EulerHS.Prelude

data CreateReq = CreateReq
  { _remarks :: Text,
    _TenantOrganizationId :: Maybe TenantOrganizationId,
    _EntityId :: Text,
    _entityType :: EntityType,
    _startTime :: LocalTime,
    _endTime :: LocalTime,
    _info :: Maybe Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CreateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CreateReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

newtype CreateRes = CreateRes
  { _blacklist :: Blacklist
  }
  deriving (Show, Generic, Default, ToSchema)

instance ToJSON CreateRes where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON CreateRes where
  parseJSON = genericParseJSON stripLensPrefixOptions

data UpdateReq = UpdateReq
  { _remarks :: Maybe Text,
    _TenantOrganizationId :: Maybe TenantOrganizationId,
    _EntityId :: Maybe Text,
    _entityType :: Maybe EntityType,
    _info :: Maybe Text,
    _startTime :: Maybe LocalTime,
    _endTime :: Maybe LocalTime
  }
  deriving (Show, Generic, ToSchema)

instance ToJSON UpdateReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON UpdateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

newtype UpdateRes = UpdateRes
  { _blacklist :: Blacklist
  }
  deriving (Show, Generic, Default, ToSchema)

instance ToJSON UpdateRes where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON UpdateRes where
  parseJSON = genericParseJSON stripLensPrefixOptions

newtype ListRes = ListRes
  { _blacklists :: [Blacklist]
  }
  deriving (Show, Generic, Default, ToSchema)

instance ToJSON ListRes where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON ListRes where
  parseJSON = genericParseJSON stripLensPrefixOptions

type GetRes = Blacklist
