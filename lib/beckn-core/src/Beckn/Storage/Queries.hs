{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Beckn.Storage.Queries where

import qualified Beckn.Storage.Common as DB
import qualified Beckn.Storage.DB.Config as DB
import Beckn.Storage.DB.Types
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified Database.Beam as B
import Database.Beam.Postgres
import qualified Database.Beam.Query.Internal as BI
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T

run :: (HasCallStack, T.JSONEx a) => L.SqlDB Pg a -> DB.FlowWithDb r (T.DBResult a)
run query = do
  connection <- DB.getOrInitConn
  L.runDB connection query

findOneWithErr ::
  ( HasCallStack,
    RunReadablePgTable table db
  ) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  DB.FlowWithDb r (table Identity)
findOneWithErr dbTable predicate =
  runSqlDB (findOne' dbTable predicate)
    >>= checkDBError
    >>= fromMaybeM (SQLResultError "Expected at least one row")

findAllOrErr ::
  ( HasCallStack,
    RunReadablePgTable table db
  ) =>
  Table table db ->
  (table (B.QExpr Postgres B.QBaseScope) -> B.QExpr Postgres B.QBaseScope Bool) ->
  DB.FlowWithDb r [table Identity]
findAllOrErr dbTable predicate = do
  runSqlDB (findAll' dbTable predicate)
    >>= checkDBError

findAllRows ::
  (HasCallStack, RunReadablePgTable table db) =>
  Table table db ->
  DB.FlowWithDb r (T.DBResult [table Identity])
findAllRows dbTable = runSqlDB $ findAllRows' dbTable

findAllRows' ::
  (HasCallStack, ReadablePgTable table db) =>
  Table table db ->
  SqlDB [table Identity]
findAllRows' dbTable = lift . L.findRows $ B.select $ B.all_ dbTable

type Scope2 = BI.QNested (BI.QNested B.QBaseScope)

type Scope3 = BI.QNested (BI.QNested (BI.QNested B.QBaseScope))

findAllWithLimitOffset ::
  (HasCallStack, RunReadablePgTable table db) =>
  Table table db ->
  Integer ->
  Integer ->
  (table (B.QExpr Postgres Scope3) -> BI.QOrd Postgres Scope3 ordering) ->
  DB.FlowWithDb r (T.DBResult [table Identity])
findAllWithLimitOffset dbTable limit offset orderBy =
  run $ L.findRows $ B.select $ B.limit_ limit $ B.offset_ offset $ B.orderBy_ orderBy $ B.all_ dbTable

findAllWithLimitOffsetWhere ::
  (HasCallStack, RunReadablePgTable table db) =>
  Table table db ->
  (table (BI.QGenExpr BI.QValueContext Postgres Scope2) -> BI.QExpr Postgres Scope2 Bool) ->
  Integer ->
  Integer ->
  (table (B.QExpr Postgres Scope3) -> BI.QOrd Postgres Scope3 ordering) ->
  DB.FlowWithDb r (T.DBResult [table Identity])
findAllWithLimitOffsetWhere dbTable predicate limit offset orderBy =
  run $ L.findRows $ B.select $ B.limit_ limit $ B.offset_ offset $ B.filter_ predicate $ B.orderBy_ orderBy $ B.all_ dbTable

aggregate ::
  ( HasCallStack,
    RunReadablePgTable table db,
    _
  ) =>
  Table table db ->
  _aggregator ->
  _predicate ->
  DB.FlowWithDb r (T.DBResult [_result])
aggregate dbTable aggregator predicate = run $ L.findRows $ B.select $ B.aggregate_ aggregator $ B.filter_ predicate $ B.all_ dbTable

findAllByJoin ::
  (HasCallStack, _) =>
  Integer ->
  Integer ->
  _orderBy ->
  _query ->
  DB.FlowWithDb r (T.DBResult [_result])
findAllByJoin limit offset orderBy =
  run . L.findRows . B.select . B.limit_ limit . B.offset_ offset . B.orderBy_ orderBy

findAllByJoinWithoutLimits ::
  (HasCallStack, _) =>
  _orderBy ->
  _query ->
  DB.FlowWithDb r (T.DBResult [_result])
findAllByJoinWithoutLimits orderBy =
  run . L.findRows . B.select . B.orderBy_ orderBy
