{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.Tracker where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time.LocalTime
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import EulerHS.Prelude
import Servant.API
import Servant.Swagger
import Types.App

data Type = DRIVER | CUSTOMER | TRIP
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Type where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres Type where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance ToParamSchema Type

instance FromHttpApiData Type where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = first T.pack . eitherDecode . BSL.fromStrict

data TrackerT f = Tracker
  { _id :: B.C f TrackerId,
    _type :: B.C f Type,
    _referenceId :: B.C f Text,
    _long :: B.C f Text,
    _lat :: B.C f Text,
    _gps :: B.C f Text,
    _createdAt :: B.C f LocalTime,
    _updatedAt :: B.C f LocalTime
  }
  deriving (Generic, B.Beamable)

type Tracker = TrackerT Identity

type TrackerPrimaryKey = B.PrimaryKey TrackerT Identity

instance B.Table TrackerT where
  data PrimaryKey TrackerT f = TrackerPrimaryKey (B.C f TrackerId)
    deriving (Generic, B.Beamable)
  primaryKey = TrackerPrimaryKey . _id

deriving instance Show Tracker

deriving instance Eq Tracker

instance ToJSON Tracker where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON Tracker where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema Tracker

insertExpression org = B.insertValues [org]

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity TrackerT)
fieldEMod =
  B.setEntityName "trip_reference"
    <> B.modifyTableFields
      B.tableModification
        { _createdAt = "created_at",
          _updatedAt = "updated_at",
          _referenceId = "reference_id"
        }
