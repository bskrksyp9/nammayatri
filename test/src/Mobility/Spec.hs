module Mobility.Spec where

import EulerHS.Prelude
import qualified Mobility.AppCancelRide as CR
import qualified Mobility.AppCaseList as CL
import qualified Mobility.DriverCancelRide as DCR
import qualified Mobility.DriversIgnoreRide as DIR
import qualified Mobility.HealthCheck as HC
import qualified Mobility.NearestDrivers as ND
import qualified Mobility.Serviceability as SRV
import qualified Mobility.SuccessFlow as SF
import Test.Tasty
import Test.Tasty.Hspec hiding (after)

mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  clSpec <- testSpec "AppCaseList" CL.spec
  sfSpec <- testSpec "SuccessFlow" SF.spec
  dirSpec <- testSpec "DriversIgnoreRide" DIR.spec
  crSpec <- testSpec "AppCancelRide" CR.spec
  dcrSpec <- testSpec "DriverCancelRide" DCR.spec
  srvSpec <- testSpec "Serviceability" SRV.spec
  ndSpec <- testSpec "NearestDriver" ND.spec
  return $
    testGroup
      "Mobility"
      [ hcSpec,
        after AllSucceed "HealthCheck" $
          testGroup
            "APIs"
            [ ndSpec,
              clSpec,
              srvSpec,
              sfSpec,
              after AllSucceed "SuccessFlow" $
                testGroup
                  "Flows"
                  [ dirSpec,
                    crSpec,
                    dcrSpec
                  ]
            ]
      ]
