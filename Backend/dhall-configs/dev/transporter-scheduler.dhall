let common = ../generic/common.dhall

let sec = ./secrets/static-offer-driver-app.dhall

let transporter = ./static-offer-driver-app.dhall

let rcfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let rccfg =
      { connectHost = "localhost"
      , connectPort = 30006
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

in  { loggerConfig =
            common.loggerConfig
        //  { logRawSql = False
            , logFilePath = "/tmp/transporter-scheduler.log"
            , prettyPrinting = True
            }
    , esqDBCfg = transporter.esqDBCfg
    , metricsPort = +8054
    , hedisCfg = rcfg
    , hedisClusterCfg = rccfg
    , hedisMigrationStage = True
    , cutOffHedisCluster = False
    , hedisPrefix = "transporter-scheduler"
    , port = +8053
    , loopIntervalSec = +5
    , expirationTime = +60
    , waitBeforeRetry = +1
    , tasksPerIteration = +20
    , graceTerminationPeriod = +10
    }
