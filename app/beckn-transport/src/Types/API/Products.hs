module Types.API.Products where

import           Data.Default
import           Data.Swagger
import           Data.Time
import           Beckn.Types.Common
import           Beckn.Types.App
import qualified Beckn.Types.Storage.Products   as Product
import           Beckn.Types.Storage.Case
import           Beckn.Types.Storage.Location
import           EulerHS.Prelude
import           Types.Storage.Driver
import           Beckn.Types.Storage.Vehicle

data ProdReq = ProdReq
  { _status   :: Maybe Product.ProductsStatus,
    _driverInfo  :: Maybe Driver,
    _vehicleInfo :: Maybe Vehicle,
    _assignedTo :: Maybe Text,
    _vehicleId :: Maybe Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ProdReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProdReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

type ProdInfoRes = Product.Products

data ProdRes = ProdRes
  { _product   :: Product.Products,
    _fromLocation  :: Location,
    _toLocation  :: Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ProdRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProdRes where
  toJSON = genericToJSON stripAllLensPrefixOptions

type ProdListRes = [ProdRes]