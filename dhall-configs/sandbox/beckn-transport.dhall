let common = ./common.dhall
let sec = ./secrets/beckn-transport.dhall

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_transporter"
  }

let pgcfg =
  { connTag = "transporterDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_transporter"
  }

let rcfg =
  { connectHost = "ec-redis-beta.bfw4iw.ng.0001.apse1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +2
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

let smsConfig =
  { sessionConfig = common.smsSessionConfig
  , credConfig = {
      username = common.smsUserName
    , password = common.smsPassword
    , otpHash = sec.smsOtpHash
    }
  , useFakeSms = None Natural
  , url = "https://http.myvfirst.com"
  , sender = "JUSPAY"
  }

let appUri = "http://beckn-app-backend-${common.branchName}.atlas:8013/v2"

let apiRateLimitOptions =
  { limit = +4
  , limitResetTimeInSec = +600
  }

let httpClientOptions =
  { timeoutMs = +2000
  , maxRetries = +3
  }

in

{ dbCfg = pgcfg
, redisCfg = rcfg
, smsCfg = smsConfig
, otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
, inviteSmsTemplate = "Welcome to the Yatri platform! Your agency ({#org#}) has added you as a driver. Start getting rides by installing the app: https://bit.ly/3wgLTcU"
, port = +8014
, bgtmPort = +8114
, metricsPort = +9999
, xGatewaySelector = "nsdl.co.in"
, xAppUri = appUri
, hostName = "juspay.in"
, nwAddress = "https://api.sandbox.beckn.juspay.in/bpp/cab/v1"
, credRegistry = common.credRegistry
, signingKeys = common.signingKeys
, caseExpiry = Some +7200
, encService = common.passetto
, fcmJsonPath = common.fcmJsonPath
, exotelCfg = None common.ExotelCfg
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, coreVersion = "0.8.2"
, domainVersion = "0.8.2"
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-transport.log"}
, signatureExpiry = common.signatureExpiry
, googleMapsUrl = "https://maps.googleapis.com/maps/api/"
, googleMapsKey = common.googleMapsKey
, fcmUrl = common.fcmUrl
, graphhopperUrl = common.graphhopperUrl
, graceTerminationPeriod = +90
, defaultRadiusOfSearch = +5000 -- meters
, driverPositionInfoExpiry = Some +600
, apiRateLimitOptions = apiRateLimitOptions
, httpClientOptions = httpClientOptions
, authTokenCacheExpiry = +600
, minimumDriverRatesCount = +5
, recalculateFareEnabled = True
, updateLocationRefreshPeriod = +5
, metricsSearchDurationTimeout = +45
, registryUrl = common.registryUrl
, registrySecrets = sec.registrySecrets
}
