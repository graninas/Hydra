module Hydra.Core.SqlDB.Interpreter where

import Hydra.Prelude

import qualified Hydra.Core.SqlDB.Language as L
import qualified Hydra.Core.Domain as D
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B


interpretSqlDBMethod
  :: D.SqlConn beM
  -> (String -> IO ())
  -> L.SqlDBMethodF beM a
  -> IO a
interpretSqlDBMethod conn logger (L.SqlDBMethod runner next) =
  next <$> runner conn logger

runSqlDBL  :: D.SqlConn beM -> (String -> IO ()) -> L.SqlDBL beM a -> IO a
runSqlDBL sqlConn logger = foldF (interpretSqlDBMethod sqlConn logger)
