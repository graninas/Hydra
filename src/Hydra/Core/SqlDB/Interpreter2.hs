--{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE PartialTypeSignatures #-}

module Hydra.Core.SqlDB.Interpreter2 where

import Hydra.Prelude

import qualified Hydra.Core.SqlDB.Language2 as L
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

runSqlDBL2  :: D.SqlConn beM -> (String -> IO ()) -> L.SqlDBL2 beM a -> IO a
runSqlDBL2 sqlConn logger = foldF (interpretSqlDBMethod sqlConn logger)
