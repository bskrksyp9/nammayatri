let common = ./common.dhall

let sec = ./secrets/bpp-dashboard.dhall

let esqDBCfg =
      { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
      , connectPort = 5432
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_bpp_dashboard_v2"
      , connectSchemaName = "atlas_bpp_dashboard"
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = esqDBCfg.connectPort
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      }

let rcfg =
      { connectHost = "beckn-redis-001.zkt6uh.ng.0001.aps1.cache.amazonaws.com"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +1
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = Some +100
      }

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let becknTransport =
      { name = common.ServerName.BECKN_TRANSPORT
      , url = "http://beckn-transport-master.atlas:8014/"
      , token = sec.becknTransportToken
      }

let driverOfferBpp =
      { name = common.ServerName.DRIVER_OFFER_BPP
      , url = "http://beckn-driver-offer-bpp-master.atlas:8016/"
      , token = sec.driverOfferBppToken
      }

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = rcfg
    , port = +8018
    , migrationPath = None Text
    , autoMigrate = common.autoMigrate
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/bpp-dashboard.log" }
    , graceTerminationPeriod = +90
    , apiRateLimitOptions
    , httpClientOptions = common.httpClientOptions
    , authTokenCacheExpiry = +600
    , registrationTokenExpiry = +365
    , encTools
    , dataServers = [ becknTransport, driverOfferBpp ]
    }
