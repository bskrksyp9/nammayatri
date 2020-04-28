module Beckn.App.Routes where

import qualified Beckn.Data.Accessor             as Accessor
import qualified Beckn.Product.Customer          as Customer
import qualified Beckn.Product.HealthCheck       as HealthCheck
import qualified Beckn.Product.Organization      as Organization
import qualified Beckn.Product.Pass              as Pass
import qualified Beckn.Product.PassApplication   as PassApplication
import qualified Beckn.Product.Registration      as Registration
import           Beckn.Types.API.Common
import           Beckn.Types.API.Customer
import           Beckn.Types.API.Organization
import           Beckn.Types.API.Pass
import           Beckn.Types.API.PassApplication
import           Beckn.Types.API.Registration
import           Beckn.Types.App
import           Data.Aeson
import qualified Data.Vault.Lazy                 as V
import           EulerHS.Prelude
import           Servant

import qualified Beckn.Types.Storage.Pass as SP
import qualified Beckn.Types.Storage.PassApplication as PA

type EPassAPIs
   = "v1" :> (Get '[ JSON] Text :<|> RegistrationAPIs :<|> PassApplicationAPIs :<|> OrganizationAPIs :<|> CustomerAPIs :<|> PassAPIs)

epassAPIs :: Proxy EPassAPIs
epassAPIs = Proxy

epassServer' :: V.Key (HashMap Text Text) -> FlowServer EPassAPIs
epassServer' key =
  HealthCheck.healthCheckApp
  :<|> registrationFlow
  :<|> passApplicationFlow
  :<|> organizationFlow
  :<|> customerFlow
  :<|> passFlow

---- Registration Flow ------
type RegistrationAPIs
   = "token"
   :> ( ReqBody '[ JSON] InitiateLoginReq
        :> Post '[ JSON] InitiateLoginRes
      :<|> Capture "tokenId" Text
          :> ReqBody '[ JSON] LoginReq
          :> Post '[ JSON] Value
      )

registrationFlow :: FlowServer RegistrationAPIs
registrationFlow =
  Registration.initiateLogin
  :<|> Registration.login
-------------------------------

---- Pass Application Flow ------
--
type PassApplicationAPIs
   = "passApplication"
   :> Header "registrationToken" Text
   :> ( ReqBody '[ JSON] CreatePassApplicationReq
        :> Post '[ JSON] PassApplicationRes
      :<|> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> QueryParams "status" PA.Status
           :> QueryParams "type" PA.PassType
           :> Get '[ JSON] ListPassApplicationRes
      :<|> Capture "passApplicationId" Text :> Get '[ JSON] PassApplicationRes
      :<|> Capture "passApplicationId" Text
           :> ReqBody '[ JSON] UpdatePassApplicationReq
           :> Post '[ JSON] PassApplicationRes
      )

passApplicationFlow registrationToken =
  PassApplication.createPassApplication registrationToken
  :<|> PassApplication.listPassApplication registrationToken
  :<|> PassApplication.getPassApplicationById registrationToken
  :<|> PassApplication.updatePassApplication registrationToken

----- Organization Flow -------
--
type OrganizationAPIs
   = "organization"
   :> Header "registrationToken" Text
   :> ( ReqBody '[ JSON] CreateOrganizationReq :> Post '[ JSON] OrganizationRes
       :<|> Capture "organizationId" Text :> Get '[ JSON] OrganizationRes
       :<|> QueryParam "limit" Int
            :> QueryParam "offset" Int
            :> QueryParam "type" Text
            :> Get '[ JSON] ListOrganizationRes
       :<|> Capture "organizationId" Text
            :> ReqBody '[ JSON] UpdateOrganizationReq
            :> Post '[ JSON] OrganizationRes
      )

organizationFlow registrationToken =
  Organization.createOrganization registrationToken
  :<|> Organization.getOrganization registrationToken
  :<|> Organization.listOrganization registrationToken
  :<|> Organization.updateOrganization registrationToken
---------------------------------

----- Customer Flow -------
type CustomerAPIs
  = "customer"
  :> Header "registrationToken" Text
  :> Capture "customerId" Text
  :> Get '[ JSON] GetCustomerRes

customerFlow registrationToken =
  Customer.getCustomerInfo registrationToken
---------------------------

------ Pass Flow ---------
type PassAPIs
  = "pass" :> Header "registrationToken" Text
  :> (Capture "passId" Text :> Get '[ JSON] PassRes
     :<|> Capture "passId" Text
          :> ReqBody '[ JSON] UpdatePassReq
          :> Post '[ JSON] PassRes
     :<|> "list"
          :> QueryParam "identifierType" PassIDType
          :> QueryParam "identifier" Text
          :> QueryParam "limit" Int
          :> QueryParam "offset" Int
          :> QueryParams "status" SP.Status
          :> QueryParams "type" SP.PassType
          :> Post '[ JSON] ListPassRes
     )

passFlow registrationToken =
  Pass.getPassById registrationToken
  :<|> Pass.updatePass registrationToken
  :<|> Pass.listPass registrationToken
