let globalCommon = ../generic/common.dhall

let appCfg = ./static-offer-driver-app.dhall

let SortMode = < ETA | IdleTime >

let shards =
      [ globalCommon.mkShard +0 "YATRI", globalCommon.mkShard +1 "YATRI_2" ]

let driverPoolBatchesCfg = { driverBatchSize = +5, maxNumberOfBatches = +3 }

in  { appCfg
    , metricsPort = +9996
    , reallocationsLimit = +5
    , driverNotificationExpiry = +2500
    , rideAllocationExpiry = +180
    , requestsNumPerIteration = +50
    , processDelay = +1000
    , shards
    , healthcheckPort = +8114
    , httpClientOptions = appCfg.httpClientOptions
    , shortDurationRetryCfg = appCfg.shortDurationRetryCfg
    , longDurationRetryCfg = appCfg.longDurationRetryCfg
    , esqDBCfg = appCfg.esqDBCfg
    , esqDBReplicaCfg = appCfg.esqDBReplicaCfg
    , hedisCfg = appCfg.hedisCfg
    , hedisClusterCfg = appCfg.hedisClusterCfg
    , hedisMigrationStage = True
    , cutOffHedisCluster = False
    , loggerConfig =
            appCfg.loggerConfig
        //  { logFilePath = "/tmp/allocation-service.log", logRawSql = False }
    , kafkaProducerCfg = appCfg.kafkaProducerCfg
    , nwAddress = appCfg.nwAddress
    , driverPoolCfg = appCfg.driverPoolCfg
    , graceTerminationPeriod = appCfg.graceTerminationPeriod
    , encTools = appCfg.encTools
    , selfUIUrl = appCfg.selfUIUrl
    , cacheConfig = appCfg.cacheConfig
    , driverPoolBatchesCfg
    }
