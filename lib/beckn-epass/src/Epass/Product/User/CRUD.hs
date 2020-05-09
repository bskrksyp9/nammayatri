{-# LANGUAGE TypeFamilies #-}

module Epass.Product.User.CRUD where

import qualified Epass.Data.Accessor                   as Accessor
import           Epass.Product.Common
import qualified Epass.Storage.Queries.Location        as Loc
import qualified Epass.Storage.Queries.Organization    as QO
import qualified Epass.Storage.Queries.User            as DB
import           Epass.Types.API.Common                as C
import           Epass.Types.API.User
import           Epass.Types.App
import           Epass.Types.Common
import qualified Epass.Types.Storage.Organization      as SO
import qualified Epass.Types.Storage.RegistrationToken as SR
import           Epass.Types.Storage.User              as Storage
import           Epass.Types.Storage.Location          as SL
import qualified Epass.Types.Storage.User              as SU
import           Epass.Utils.Common
import           Epass.Utils.Extra
import           Epass.Utils.Routes
import           Epass.Utils.Storage
import           Data.Aeson
import           Data.Default
import qualified Data.List                             as List
import           Data.Time
import qualified Database.Beam.Schema.Tables           as B
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import           Servant

create :: Maybe Text -> CreateReq -> FlowHandler CreateRes
create regToken CreateReq {..} =
  withFlowHandler $ do
    verifyToken regToken
    id <- generateGUID
    loc <- Loc.findLocationWithErr _LocationId
    user <- userInfo id
    DB.create user
    eres <- DB.findById id
    locInfo <- getLocationInfo _LocationId
    return $ mkUInfo user locInfo
  where
    userInfo id = do
      now <- getCurrTime
      return
        Storage.User
          { _id = id
          , _verified = False
          , _status = INACTIVE
          , _info = Nothing
          , _createdAt = now
          , _updatedAt = now
          , _username = Nothing
          , _email = Nothing
          , _TenantOrganizationId = Nothing -- TODO : Fix this
          , ..
          }

list ::
     Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> Maybe LocateBy
  -> [Text]
  -> [Role]
  -> FlowHandler ListRes
list regToken offsetM limitM locateM locate roleM =
  withFlowHandler $ do
    reg <- verifyToken regToken
    when (SR._entityType reg == SR.CUSTOMER) $ do
      L.throwException $ err400 {errBody = "UNAUTHORIZED_CUSTOMER"}
    user <- DB.findById (UserId $ SR._EntityId reg)
    orgM <- QO.findOrganizationById (SU._OrganizationId user)
    when (isNothing orgM) $
      L.throwException $ err400 {errBody = "NO_ORGANIZATION_FOUND"}
    let org = fromJust orgM
    getUsers limitM offsetM locateM roleM locate user org

getUsers ::
     Maybe Int
  -> Maybe Int
  -> Maybe LocateBy
  -> [Role]
  -> [Text]
  -> SU.User
  -> SO.Organization
  -> L.Flow ListRes
getUsers offsetM limitM locateM role locate user org = do
  case SU._role user of
    ADMIN ->
      case locateM of
        Just LCITY -> cityLevelUsers limitM offsetM role locate
        Just LDISTRICT -> districtLevelUsers limitM offsetM role locate
        Just LWARD -> wardLevelUsers limitM offsetM role locate
        _ ->
          DB.findAllWithLimitOffsetByRole limitM offsetM role >>=
          return . ListRes
    CITYLEVEL -> do
      allLocations <-
        Loc.findByStOrDistrict offsetM limitM LCITY (SO._city org)
      case locateM of
        Just LCITY ->
          if List.null locate || elem (SO._city org) locate
            then cityLevelUsers limitM offsetM role [(SO._city org)]
            else L.throwException $ err400 {errBody = "UNAUTHORIZED"}
        Just LDISTRICT -> do
          let dists = catMaybes $ map SL._district allLocations
          let locateD =
                if List.null locate
                  then dists
                  else filter (flip elem dists) locate
          districtLevelUsers limitM offsetM role locateD
        Just LWARD -> do
          let wards = catMaybes $ map SL._ward allLocations
          let locateW =
                if List.null locate
                  then wards
                  else filter (flip elem wards) locate
          wardLevelUsers limitM offsetM role locateW
        _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}
    DISTRICTLEVEL -> do
      let district = fromJust $ SO._district org
      allLocations <-
        Loc.findByStOrDistrict offsetM limitM LDISTRICT district
      case locateM of
        Just LDISTRICT ->
          if List.null locate || elem district locate
            then districtLevelUsers limitM offsetM role [district]
            else L.throwException $ err400 {errBody = "UNAUTHORIZED"}
        Just LWARD -> do
          let wards = catMaybes $ map SL._ward allLocations
          let locateW =
                if List.null locate
                  then wards
                  else filter (flip elem wards) locate
          wardLevelUsers limitM offsetM role locateW
        _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}
    WARDLEVEL -> do
      let ward = fromJust $ SO._ward org
      case locateM of
        Just LWARD ->
          if List.null locate || List.elem ward locate
            then wardLevelUsers limitM offsetM role [ward]
            else L.throwException $ err400 {errBody = "UNAUTHORIZED"}
        _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}
    _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}

cityLevelUsers :: Maybe Int -> Maybe Int -> [Role] -> [Text] -> L.Flow ListRes
cityLevelUsers limitM offsetM r cities =
  QO.listOrganizations
    Nothing
    Nothing
    mempty
    mempty
    cities
    mempty
    mempty
    empty
    empty
    Nothing >>=
  DB.findAllWithLimitOffsetBy limitM offsetM r . map SO._id >>=
  return . ListRes

districtLevelUsers ::
     Maybe Int -> Maybe Int -> [Role] -> [Text] -> L.Flow ListRes
districtLevelUsers limitM offsetM r districts =
  QO.listOrganizations
    Nothing
    Nothing
    mempty
    mempty
    mempty
    districts
    mempty
    empty
    empty
    Nothing >>=
  DB.findAllWithLimitOffsetBy limitM offsetM r . map SO._id >>=
  return . ListRes

wardLevelUsers :: Maybe Int -> Maybe Int -> [Role] -> [Text] -> L.Flow ListRes
wardLevelUsers limitM offsetM r wards =
  QO.listOrganizations
    Nothing
    Nothing
    mempty
    mempty
    mempty
    mempty
    wards
    empty
    empty
    Nothing >>=
  DB.findAllWithLimitOffsetBy limitM offsetM r . map SO._id >>=
  return . ListRes

get :: Maybe Text -> UserId -> FlowHandler GetRes
get regToken userId =
  withFlowHandler $ do
    verifyToken regToken
    user@User {..} <- DB.findById userId
    locInfo <- getLocationInfo _LocationId
    return $ mkUInfo user locInfo

update :: Maybe Text -> UserId -> UpdateReq -> FlowHandler UpdateRes
update regToken userId UpdateReq {..} =
  withFlowHandler $
  do verifyToken regToken
     DB.update userId _status _name _role
     UpdateRes <$> DB.findById userId

delete :: Maybe RegistrationTokenText -> UserId -> FlowHandler Ack
delete regToken userId =
  withFlowHandler $ do
    verifyToken regToken
    DB.deleteById userId
    sendAck

listRoles :: Maybe RegistrationTokenText -> FlowHandler [Role]
listRoles regToken =
  withFlowHandler $ do
    verifyToken regToken
    pure $ enumFrom minBound

-- Transformers
mkUInfo :: User -> C.LocationInfo -> UserInfo
mkUInfo user locInfo = UserInfo {_user = user, _locationInfo = locInfo}
