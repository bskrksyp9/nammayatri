module Idfy.WebhookHandler where

import Beckn.Tools.Metrics.CoreMetrics.Types
import Beckn.Types.Flow
import Beckn.Utils.Common
import Beckn.Utils.IOLogging
import qualified Data.Text as T
import Data.Time.Format
import EulerHS.Prelude
import Idfy.Auth
import qualified Idfy.External.Flow as EF
import Idfy.Types.IdfyConfig
import Idfy.Types.Request
import Idfy.Types.Response

webhookHandler ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    HasField "idfyCfg" a IdfyConfig
  ) =>
  (VerificationResponse -> FlowR a AckResponse) ->
  (VerificationResponse -> FlowR a AckResponse) ->
  Maybe Text ->
  VerificationResponse ->
  FlowHandlerR a AckResponse
webhookHandler verifyDL_ verifyRC_ secret result = withFlowHandlerAPI $ do
  void $ verifyAuth secret
  void $ verifyDL_ result
  void $ verifyRC_ result
  pure Ack

verifyDL ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    CoreMetrics (FlowR a),
    HasField "idfyCfg" a IdfyConfig
  ) =>
  Text ->
  UTCTime ->
  FlowR a IdfySuccess
verifyDL dlId dob = do
  idfyConfig <- asks (.idfyCfg)
  let dobDay = T.pack $ formatTime defaultTimeLocale "%F" dob
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        IdfyRequest
          { _data =
              DLVerificationData
                { id_number = dlId,
                  date_of_birth = dobDay
                },
            ..
          }
  EF.verifyDLAsync idfyConfig.api_key idfyConfig.account_id idfyConfig.url req

verifyRC ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    CoreMetrics (FlowR a),
    HasField "idfyCfg" a IdfyConfig
  ) =>
  Text ->
  FlowR a IdfySuccess
verifyRC rcId = do
  idfyConfig <- asks (.idfyCfg)
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        IdfyRequest
          { _data = RCVerificationData rcId Nothing,
            ..
          }
  EF.verifyRCAsync idfyConfig.api_key idfyConfig.account_id idfyConfig.url req

validateImage ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    CoreMetrics (FlowR a),
    HasField "idfyCfg" a IdfyConfig
  ) =>
  Text ->
  Text ->
  FlowR a ImageValidateResponse
validateImage doc1 doctype = do
  idfyConfig <- asks (.idfyCfg)
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        IdfyRequest
          { _data =
              ValidateRequest
                { document1 = doc1,
                  doc_type = doctype
                },
            ..
          }
  EF.validateImage idfyConfig.api_key idfyConfig.account_id idfyConfig.url req

extractRCImage ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    CoreMetrics (FlowR a),
    HasField "idfyCfg" a IdfyConfig
  ) =>
  Text ->
  Maybe Text ->
  FlowR a RCExtractResponse
extractRCImage doc1 mbdoc2 = do
  idfyConfig <- asks (.idfyCfg)
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        IdfyRequest
          { _data =
              ExtractRequest
                { document1 = doc1,
                  document2 = mbdoc2
                },
            ..
          }
  EF.extractRCImage idfyConfig.api_key idfyConfig.account_id idfyConfig.url req

extractDLImage ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    CoreMetrics (FlowR a),
    HasField "idfyCfg" a IdfyConfig
  ) =>
  Text ->
  Maybe Text ->
  FlowR a DLExtractResponse
extractDLImage doc1 mbdoc2 = do
  idfyConfig <- asks (.idfyCfg)
  task_id <- generateGUID
  group_id <- generateGUID
  let req =
        IdfyRequest
          { _data =
              ExtractRequest
                { document1 = doc1,
                  document2 = mbdoc2
                },
            ..
          }
  EF.extractDLImage idfyConfig.api_key idfyConfig.account_id idfyConfig.url req