{-# LANGUAGE OverloadedLabels #-}

module Product.Case where

import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.Location as Location
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Products as Products
import Types.API.Case as API
import Utils.Common (verifyToken)

status ::
  SR.RegistrationToken ->
  CaseId ->
  FlowHandler StatusRes
status SR.RegistrationToken {..} caseId = withFlowHandler $ do
  case_ <- Case.findById caseId
  cpr <- ProductInstance.findAllByCaseId (Case._id case_)
  products <- Products.findAllByIds (ProductInstance._productId <$> cpr)
  fromLocation <-
    fromMaybeM500 "Could not find from location"
      =<< Location.findLocationById (LocationId $ case_ ^. #_fromLocationId)
  toLocation <-
    fromMaybeM500 "Could not find to location"
      =<< Location.findLocationById (LocationId $ case_ ^. #_toLocationId)
  return $ StatusRes case_ products fromLocation toLocation

list ::
  SR.RegistrationToken ->
  Case.CaseType ->
  [Case.CaseStatus] ->
  Maybe Integer ->
  Maybe Integer ->
  FlowHandler ListRes
list token caseType statuses mlimit moffset = withFlowHandler $ do
  person <-
    Person.findById (PersonId $ SR._EntityId token)
      >>= fromMaybeM500 "Could not find user"
  ListRes
    <$> ( Case.findAllByTypeAndStatuses (person ^. #_id) caseType statuses mlimit moffset
            >>= traverse mapProductInstance
        )
  where
    mapProductInstance case_@Case.Case {..} = do
      prds <- ProductInstance.findAllProductsByCaseId _id
      fromLocation <- Location.findLocationById $ LocationId _fromLocationId
      toLocation <- Location.findLocationById $ LocationId _toLocationId
      return $ API.ProductInstance case_ prds fromLocation toLocation
