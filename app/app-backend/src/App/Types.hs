{-# LANGUAGE TypeApplications #-}

module App.Types where

import Beckn.External.Exotel.Types (ExotelCfg)
import Beckn.Sms.Config (SmsConfig)
import Beckn.Storage.DB.Config (DBConfig)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Utils.Dhall (FromDhall, ZL (..), z)
import EulerHS.Prelude
import Servant.Client (BaseUrl, Scheme)

data AppEnv = AppEnv
  { dbCfg :: DBConfig,
    smsCfg :: SmsConfig,
    port :: Int,
    metricsPort :: Int,
    xGatewayUri :: BaseUrl,
    xGatewayApiKey :: Maybe Text,
    xGatewaySelector :: Maybe Text,
    xGatewayNsdlUrl :: Maybe BaseUrl,
    nsdlUsername :: Maybe Text,
    nsdlPassword :: Maybe Text,
    xProviderUri :: BaseUrl,
    bapSelfId :: Maybe Text,
    bapNwAddress :: Maybe Text,
    searchConfirmExpiry :: Maybe Integer,
    searchCaseExpiry :: Maybe Integer,
    cronAuthKey :: Maybe CronAuthKey,
    encService :: (String, Word16),
    fcmJsonPath :: Maybe Text,
    exotelCfg :: Maybe ExotelCfg,
    migrationPath :: Maybe FilePath,
    autoMigrate :: Bool
  }
  deriving (Generic, FromDhall)

type Env = EnvR AppEnv

type Flow = FlowR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

tyEnv :: ZL '[Scheme, ExotelCfg, BaseUrl]
tyEnv = z @Scheme "UrlScheme" $ z @ExotelCfg "ExotelCfg" $ z @BaseUrl "BaseUrl" Z
