{-# OPTIONS_GHC -Wno-unused-matches #-}
module Product.DriveronBoarding.DriverDrivingLicense where
import Types.API.Driveronboarding.DriverDrivingLicense
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Prelude
import Environment


registrationHandler :: DriverDrivingLicenseReq -> FlowHandler APISuccess
registrationHandler req = pure Success

 