module Hydra.Core.SqlDB.Interpreter where

import Hydra.Prelude

import qualified Hydra.Core.SqlDB.Language as L
import qualified Hydra.Core.Domain as D


interpretSqlDBMethod
  :: D.NativeSqlConn
  -> (String -> IO ())
  -> L.SqlDBMethodF beM a
  -> IO a
interpretSqlDBMethod nativeConn logger (L.SqlDBMethod runner next) =
  next <$> runner nativeConn logger

runSqlDBL  :: D.NativeSqlConn -> (String -> IO ()) -> L.SqlDBL beM a -> IO a
runSqlDBL nativeConn logger = foldFree (interpretSqlDBMethod nativeConn logger)
