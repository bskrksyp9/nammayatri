module Storage.Queries.Person where

import           Database.Beam                    ((&&.), (<-.), (==.), (||.))
import           EulerHS.Prelude                  hiding (id)

import qualified Storage.Queries            as DB
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Types.Storage.DB                 as DB
import qualified Beckn.Types.Storage.Person       as Storage
import           Beckn.Utils.Common
import           Beckn.Utils.Extra
import           Data.Time
import qualified Database.Beam                    as B
import qualified EulerHS.Language                 as L
import qualified EulerHS.Types                    as T

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.PersonT)
dbTable = DB._person DB.transporterDb

create :: Storage.Person -> L.Flow ()
create Storage.Person {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Person {..})
    >>= either DB.throwDBError pure

findPersonById ::
  PersonId -> L.Flow (Maybe Storage.Person)
findPersonById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Person {..} = (_id ==. B.val_ id)

findAllByOrgIds ::
  [Storage.Role] -> [Text] -> L.Flow [Storage.Person]
findAllByOrgIds roles orgIds = do
    DB.findAllOrErr dbTable (predicate roles orgIds)
  where
    predicate roles orgIds Storage.Person {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _role `B.in_` (B.val_ <$> roles) ||. complementVal roles
        , _organizationId `B.in_` ((\x -> B.val_ $ Just x) <$> orgIds) ||. complementVal orgIds
        ]

complementVal l
  | (null l) = B.val_ True
  | otherwise = B.val_ False


findByIdentifier ::
  Storage.IdentifierType -> Text -> L.Flow (Maybe Storage.Person)
findByIdentifier idType mb =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Person {..} =
      _identifierType ==. B.val_ idType
        &&. _mobileNumber ==. B.val_ (Just mb)

findByRoleAndIdentifier ::
  Storage.Role -> Storage.IdentifierType -> Text -> L.Flow (Maybe Storage.Person)
findByRoleAndIdentifier role idType identifier =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Person {..} =
      _role ==. B.val_ role
        &&. _mobileNumber ==. B.val_ (Just identifier)

updatePersonRec :: PersonId -> Storage.Person -> L.Flow ()
updatePersonRec personId person = do
  now <- getCurrentTimeUTC
  DB.update dbTable (setClause person now) (predicate personId)
    >>= either DB.throwDBError pure
  where
    setClause person n Storage.Person {..} =
      mconcat
        [
          _firstName <-. B.val_ (Storage._firstName person)
          , _middleName <-. B.val_ (Storage._middleName person)
          , _lastName <-. B.val_ (Storage._lastName person)
          , _fullName <-. B.val_ (Storage._fullName person)
          , _role <-. B.val_ (Storage._role person)
          , _gender <-. B.val_ (Storage._gender person)
          , _email <-. B.val_ (Storage._email person)
          , _mobileNumber <-. B.val_ (Storage._mobileNumber person)
          , _mobileCountryCode <-. B.val_ (Storage._mobileCountryCode person)
          , _identifier <-. B.val_ (Storage._identifier person)
          , _rating <-. B.val_ (Storage._rating person)
          , _deviceToken <-. B.val_ (Storage._deviceToken person)
          , _udf1 <-. B.val_ (Storage._udf1 person)
          , _udf2 <-. B.val_ (Storage._udf2 person)
          , _organizationId <-. B.val_ (Storage._organizationId person)
          , _description <-. B.val_ (Storage._description person)
          , _updatedAt <-. B.val_ n
        ]
    predicate id Storage.Person {..} = _id ==. B.val_ id

updatePerson :: PersonId -> Bool -> Text -> Storage.IdentifierType -> Maybe Text -> L.Flow ()
updatePerson personId verified identifier identifierType mobileNumber = do
  now <- getCurrentTimeUTC
  DB.update dbTable (setClause identifier identifierType mobileNumber verified now) (predicate personId)
    >>= either DB.throwDBError pure
  where
    setClause i it mn v n Storage.Person {..} =
      mconcat
        [ _identifier <-. B.val_ (Just i),
          _identifierType <-. B.val_ it,
          _mobileNumber <-. B.val_ mn,
          _verified <-. B.val_ v,
          _updatedAt <-. B.val_ n
        ]
    predicate id Storage.Person {..} = _id ==. B.val_ id

update ::
  PersonId ->
  Storage.Status ->
  Maybe Text ->
  Maybe Text ->
  Maybe Storage.Role ->
  L.Flow ()
update id status nameM emailM roleM = do
  (currTime :: LocalTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status nameM emailM roleM currTime)
    (predicate id)
    >>= either DB.throwDBError pure
  where
    setClause status nameM emailM roleM currTime Storage.Person {..} =
      mconcat
        ( [ _status <-. B.val_ status,
            _updatedAt <-. B.val_ currTime
          ]
            <> (\name -> [_fullName <-. B.val_ name]) nameM
            <> (\email -> [_email <-. B.val_ email]) emailM
            <> maybe [] (\role -> [_role <-. B.val_ role]) roleM
        )
    predicate id Storage.Person {..} = _id ==. B.val_ id
