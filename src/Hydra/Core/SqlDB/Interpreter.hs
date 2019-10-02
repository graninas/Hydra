module Hydra.Core.SqlDB.Interpreter where

import           Hydra.Prelude
import qualified Data.Map as Map

import qualified Hydra.Core.Language as L
import qualified Hydra.Core.RLens    as RLens
import qualified Hydra.Core.Runtime  as R
import qualified Hydra.Core.Domain   as D
import           Hydra.Core.Logger.Impl.HsLoggerInterpreter (runLoggerL)

import qualified Database.Beam.Sqlite as SQLite
import           Database.Beam.Query (runSelectReturningList)
import           Database.Beam.Sqlite.Connection (runBeamSqliteDebug)
import           Database.Beam.Sqlite (Sqlite)

import           Unsafe.Coerce (unsafeCoerce)

-- aggregate_ (\t -> ( as_ @Double @QAggregateContext $ customExpr_ (\bytes ms -> "regr_intercept(" <> bytes <> ", " <> ms <> ")") (trackBytes t) (trackMilliseconds t)
--                   , as_ @Double @QAggregateContext $ customExpr_ (\bytes ms -> "regr_slope(" <> bytes <> ", " <> ms <> ")") (trackBytes t) (trackMilliseconds t) )) $
--      all_ (track chinookDb)

-- SELECT regr_intercept(("t0"."Bytes"), ("t0"."Milliseconds")) AS "res0",
--        regr_slope(("t0"."Bytes"), ("t0"."Milliseconds")) AS "res1"
-- FROM "Track" AS "t0"

-- runSelectReturningList
  -- :: (MonadBeam be m, BeamSqlBackend be, FromBackendRow be a)
  -- => SqlSelect be a -> m [a]

-- runBeamSqliteDebug putStrLn conn $ do
--   users <- runSelectReturningList $ select allUsers
--   mapM_ (liftIO . putStrLn . show) users

-- data SqlDBF be next where
--   RunBeamSelect :: (BeamSqlBackend be, FromBackendRow be a) => SqlSelect be a -> (D.DBResult a -> next) -> SqlDBF next

-- TODO: get rid of unsafeCoerse
-- TODO: get rid of Sqlite

interpretSQLiteDBF :: R.CoreRuntime -> D.DBName -> L.SqlDBF Sqlite a -> IO a
interpretSQLiteDBF coreRt dbName (L.RunBeamSelect selectQ next) = do
  let loggerHandle = coreRt ^. RLens.loggerRuntime . RLens.hsLoggerHandle
  let loggerF msg = runLoggerL loggerHandle $ L.logDebug $ toText msg
  let connsVar = coreRt ^. RLens.sqliteConns
  conns <- atomically $ readTMVar connsVar
  case Map.lookup dbName conns of
    Nothing -> pure $ next $ Left $ D.DBError D.ConnectionIsDead $ toText dbName
    Just connVar -> do
      conn <- readMVar connVar
      rs <- SQLite.runBeamSqliteDebug loggerF conn
              $ runSelectReturningList
              $ selectQ
      error "Not implemented...."


runSqlDBL :: R.CoreRuntime -> D.SqlDBHandle Sqlite -> L.SqlDBL Sqlite a -> IO a
runSqlDBL coreRt (D.SQLiteHandle dbName) act =
  foldFree (interpretSQLiteDBF coreRt dbName) act
