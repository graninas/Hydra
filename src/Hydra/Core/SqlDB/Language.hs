{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Hydra.Core.SqlDB.Language where

import           Hydra.Prelude

import qualified Hydra.Core.Domain.DB   as D
import qualified Hydra.Core.Domain.SQLDB as D

import           Language.Haskell.TH.MakeFunctor (makeFunctorInstance)

import Database.Beam
import Database.Beam.Sqlite

data SqlDBF next where
  RunBeam :: String -> (D.DBResult a -> next) -> SqlDBF next

makeFunctorInstance ''SqlDBF

type SqlDBL db = Free SqlDBF

rawQuery :: String -> SqlDBL (D.DBResult a)
rawQuery rawQuery = liftF $ RawQuery rawQuery id
