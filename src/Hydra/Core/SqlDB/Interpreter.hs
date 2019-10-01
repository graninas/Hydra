module Hydra.Core.SqlDB.Interpreter where

import           Hydra.Prelude

import qualified Hydra.Core.Language as L
import qualified Hydra.Core.RLens    as RLens
import qualified Hydra.Core.Runtime  as R
import qualified Hydra.Core.Domain   as D

-- aggregate_ (\t -> ( as_ @Double @QAggregateContext $ customExpr_ (\bytes ms -> "regr_intercept(" <> bytes <> ", " <> ms <> ")") (trackBytes t) (trackMilliseconds t)
--                   , as_ @Double @QAggregateContext $ customExpr_ (\bytes ms -> "regr_slope(" <> bytes <> ", " <> ms <> ")") (trackBytes t) (trackMilliseconds t) )) $
--      all_ (track chinookDb)

-- SELECT regr_intercept(("t0"."Bytes"), ("t0"."Milliseconds")) AS "res0",
--        regr_slope(("t0"."Bytes"), ("t0"."Milliseconds")) AS "res1"
-- FROM "Track" AS "t0"


interpretSqlDBF :: db -> L.SqlDBF a -> IO a
-- interpretSqlDBF db (L.RawQuery rawQ next) = error "not implemented"
interpretSqlDBF db (L.RunBeam _ next) = error "not implemented"

runSqlDBL :: db -> L.SqlDBL a -> IO a
runSqlDBL conn act = foldFree (interpretSqlDBF conn) act
