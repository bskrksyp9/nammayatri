let common = ./common.dhall

let sec = ./secrets/rider-app.dhall

let esqDBCfg =
      { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
      , connectPort = 5432
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_app"
      , connectSchemaName = "atlas_app"
      }

let esqDBReplicaCfg =
      { connectHost =
          "beckn-integ-v2-r1.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
      , connectPort = esqDBCfg.connectPort
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      }

let hcfg =
      { connectHost = "beckn-redis-001.zkt6uh.ng.0001.aps1.cache.amazonaws.com"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +2
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = Some +100
      }

let smsConfig =
      { sessionConfig = common.smsSessionConfig
      , credConfig =
        { username = common.smsUserName
        , password = common.smsPassword
        , otpHash = sec.smsOtpHash
        }
      , useFakeSms = Some 7891
      , url = "https://http.myvfirst.com"
      , sender = "JUSPAY"
      }

let InfoBIPConfig =
      { username = common.InfoBIPConfig.username
      , password = common.InfoBIPConfig.password
      , token = common.InfoBIPConfig.token
      , url = "https://gye1yw.api.infobip.com"
      , sender = "JUSPAY"
      }

let nsdlGatewayUrl = "https://pilot-gateway-1.beckn.nsdl.co.in/komn"

let juspayGatewayUrl = "https://api.sandbox.beckn.juspay.in/gateway/v1"

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let searchRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let slackCfg =
      { channelName = "#beckn-driver-onboard-test"
      , slackToken = common.slackToken
      }

let httpClientOptions = { timeoutMs = +2000 }

let shortDurationRetryCfg = { maxRetries = +3, baseCoefficient = +2 }

let longDurationRetryCfg = { maxRetries = +3, baseCoefficient = +4 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let kafkaProducerCfg = { brokers = [] : List Text }

let rideConfig =
      { driverReachedDistance = +100, driverOnTheWayNotifyExpiry = +3600 }

let cacheConfig = { configsExpTime = +86400 }

let cacheTranslationConfig = { expTranslationTime = +3600 }

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = hcfg
    , smsCfg = smsConfig
    , infoBIPCfg = InfoBIPConfig
    , otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
    , port = +8013
    , metricsPort = +9999
    , hostName = "juspay.in"
    , selfUIUrl = "https://api.sandbox.beckn.juspay.in/bap/v2/"
    , bapSelfIds =
      { cabs = "api.sandbox.beckn.juspay.in/bap/cab/v1"
      , metro = "api.sandbox.beckn.juspay.in/bap/metro/v1"
      }
    , bapSelfURIs =
      { cabs = "https://api.sandbox.beckn.juspay.in/bap/cab/v1/"
      , metro = "https://api.sandbox.beckn.juspay.in/bap/metro/v1/"
      }
    , bapSelfUniqueKeyIds = { cabs = "35", metro = "35" }
    , signingKey = sec.signingKey
    , signatureExpiry = common.signatureExpiry
    , searchRequestExpiry = Some +600
    , exotelCfg = Some common.exotelCfg
    , migrationPath = None Text
    , autoMigrate = common.autoMigrate
    , coreVersion = "0.9.3"
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/rider-app.log" }
    , googleTranslateUrl = common.googleTranslateUrl
    , googleTranslateKey = common.googleTranslateKey
    , metricsSearchDurationTimeout = +45
    , graceTerminationPeriod = +90
    , apiRateLimitOptions
    , searchRateLimitOptions
    , slackCfg
    , searchLimitExceedNotificationTemplate =
        "Customer with {#cust-id#} is exceeding the search limit."
    , httpClientOptions
    , shortDurationRetryCfg
    , longDurationRetryCfg
    , authTokenCacheExpiry = +600
    , registryUrl = common.registryUrl
    , gatewayUrl = nsdlGatewayUrl
    , disableSignatureAuth = False
    , encTools
    , kafkaProducerCfg
    , rideCfg = rideConfig
    , cacheConfig
    , dashboardToken = sec.dashboardToken
    , cacheTranslationConfig
    }