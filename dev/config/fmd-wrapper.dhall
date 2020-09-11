let common = ./generic/common.dhall
let sec = ./secrets/fmd-wrapper.dhall

let postgresConfig =
  { connectHost = "localhost"
  , connectPort = 5436
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_fmd_wrapper"
  }

let pgcfg =
  { connTag = "fmdWrapperDb"
  , pgConfig = postgresConfig
  , poolConfig = (./generic/common.dhall).defaultPoolConfig
  , schemaName = "atlas_fmd_wrapper"
  }

let rcfg =
  { connectHost = "localhost"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +0
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

let dunzoConfig =
  { dzUrl = "apis-staging.dunzo.in"
  , dzTokenUrl = "http://d4b.dunzodev.in:9016"
  , dzBPId = "fmd-wrapper.dunzo"
  , dzBPNwAddress = "http://localhost:8018/v1"
  , paymentPolicy = sec.paymentPolicy
  , payee = sec.payee
  }

in

{ dbCfg = pgcfg
, redisCfg = rcfg
, port = +8018
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, logRawSql = True
, coreVersion = "0.8.0"
, domainVersion = "0.8.2"
, dzConfig = dunzoConfig
}
