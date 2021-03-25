module Flow.RideAPI.StartRide where

import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common (servantJsonError400, servantJsonError500)
import EulerHS.Prelude
import qualified Fixtures
import qualified Product.RideAPI.Handlers.StartRide as StartRide
import Servant.Server (ServerError)
import qualified Servant.Server.Internal as S
import Test.Tasty
import Test.Tasty.HUnit

handle :: StartRide.ServiceHandle IO
handle =
  StartRide.ServiceHandle
    { findPersonById = \_personid -> pure Fixtures.defaultDriver,
      findPIById = \piId ->
        if piId == Id "1"
          then pure rideProductInstance
          else pure searchProductInstance,
      findPIsByParentId = \_parentId -> pure [rideProductInstance, trackerProductInstance],
      findCaseByIdsAndType = \_caseIds caseType ->
        if caseType == Case.LOCATIONTRACKER
          then pure trackerCase
          else pure rideCase,
      startRide = \_piIds _trackerCaseId _orderCaseId -> pure (),
      notifyBAPRideStarted = \_searchPi _orderPi -> pure ()
    }

rideProductInstance :: ProductInstance.ProductInstance
rideProductInstance =
  Fixtures.defaultProductInstance
    { ProductInstance._status = ProductInstance.CONFIRMED,
      ProductInstance._parentId = Just "2",
      ProductInstance._udf4 = Just "otp"
    }

searchProductInstance :: ProductInstance.ProductInstance
searchProductInstance =
  Fixtures.defaultProductInstance
    { ProductInstance._id = "2",
      ProductInstance._caseId = "2",
      ProductInstance._type = Case.RIDESEARCH,
      ProductInstance._status = ProductInstance.CONFIRMED
    }

trackerProductInstance :: ProductInstance.ProductInstance
trackerProductInstance =
  Fixtures.defaultProductInstance
    { ProductInstance._id = "3",
      ProductInstance._caseId = "3",
      ProductInstance._type = Case.LOCATIONTRACKER,
      ProductInstance._status = ProductInstance.CONFIRMED
    }

trackerCase :: Case.Case
trackerCase =
  Fixtures.defaultCase
    { Case._status = Case.CONFIRMED
    }

rideCase :: Case.Case
rideCase =
  Fixtures.defaultCase
    { Case._id = "2",
      Case._type = Case.LOCATIONTRACKER,
      Case._status = Case.CONFIRMED
    }

startRide :: TestTree
startRide =
  testGroup
    "Starting ride"
    [ successfulStartByDriver,
      successfulStartByAdmin,
      failedStartRequestedByDriverNotAnOrderExecutor,
      failedStartRequestedByNotDriverAndNotAdmin,
      failedStartWhenProductInstanceStatusIsWrong,
      failedStartWhenRideDoesNotHaveParentProductInstance,
      failedStartWhenRideMissingOTP,
      failedStartWithWrongOTP
    ]

runHandler :: StartRide.ServiceHandle IO -> Text -> Text -> Text -> IO (Either ServerError APIResult.APIResult)
runHandler handle requestorId rideId otp = try $ StartRide.startRideHandler handle requestorId rideId otp

successfulStartByDriver :: TestTree
successfulStartByDriver =
  testCase "Start successfully if requested by driver executor" $ do
    result <- runHandler handle "1" "1" "otp"
    result @?= Right APIResult.Success

successfulStartByAdmin :: TestTree
successfulStartByAdmin =
  testCase "Start successfully if requested by admin" $ do
    result <- runHandler handleCase "1" "1" "otp"
    result @?= Right APIResult.Success
  where
    handleCase =
      handle
        { StartRide.findPersonById = \personId ->
            pure
              Fixtures.defaultDriver
                { Person._id = "adminId",
                  Person._role = Person.ADMIN
                }
        }

failedStartRequestedByDriverNotAnOrderExecutor :: TestTree
failedStartRequestedByDriverNotAnOrderExecutor = do
  testCase "Fail ride starting if requested by driver not an order executor" $ do
    result <- runHandler handleCase "2" "1" "otp"
    result @?= Left (servantJsonError400 "NOT_AN_EXECUTOR_OF_THIS_RIDE" "You are not an executor of this ride.")
  where
    handleCase =
      handle
        { StartRide.findPersonById = \personId ->
            pure
              Fixtures.defaultDriver
                { Person._id = "2"
                }
        }

failedStartRequestedByNotDriverAndNotAdmin :: TestTree
failedStartRequestedByNotDriverAndNotAdmin = do
  testCase "Fail ride starting if requested by not a driver and not an admin" $ do
    result <- runHandler handleCase "1" "1" "otp"
    result @?= Left (servantJsonError400 "NOT_AN_EXECUTOR_OF_THIS_RIDE" "You are not an executor of this ride.")
  where
    handleCase =
      handle
        { StartRide.findPersonById = \personId ->
            pure
              Fixtures.defaultDriver
                { Person._role = Person.MANAGER
                }
        }

failedStartWhenProductInstanceStatusIsWrong :: TestTree
failedStartWhenProductInstanceStatusIsWrong = do
  testCase "Fail ride starting if ride has wrong status" $ do
    result <- runHandler handleCase "1" "1" "otp"
    result @?= Left (servantJsonError400 "INVALID_RIDE_STATUS" "Ride cannot be started.")
  where
    handleCase =
      handle
        { StartRide.findPIById = \piId ->
            pure
              rideProductInstance
                { ProductInstance._status = ProductInstance.COMPLETED
                }
        }

failedStartWhenRideDoesNotHaveParentProductInstance :: TestTree
failedStartWhenRideDoesNotHaveParentProductInstance = do
  testCase "Fail ride starting if ride does not have parent ProductInstance" $ do
    result <- runHandler handleCase "1" "1" "otp"
    result @?= Left (servantJsonError400 "INVALID_RIDE_ID" "Invalid ride id.")
  where
    handleCase =
      handle
        { StartRide.findPIById = \piId ->
            pure
              rideProductInstance
                { ProductInstance._parentId = Nothing
                }
        }

failedStartWhenRideMissingOTP :: TestTree
failedStartWhenRideMissingOTP = do
  testCase "Fail ride starting if ride does not have OTP" $ do
    result <- runHandler handleCase "1" "1" "otp"
    result @?= Left (servantJsonError500 "RIDE_OTP_MISSING" "Ride does not have OTP.")
  where
    handleCase =
      handle
        { StartRide.findPIById = \piId ->
            pure
              rideProductInstance
                { ProductInstance._udf4 = Nothing
                }
        }

failedStartWithWrongOTP :: TestTree
failedStartWithWrongOTP = do
  testCase "Fail ride starting if OTP is wrong" $ do
    result <- runHandler handle "1" "1" "otp2"
    result @?= Left (servantJsonError400 "INCORRECT_RIDE_OTP" "Input OTP is wrong.")
