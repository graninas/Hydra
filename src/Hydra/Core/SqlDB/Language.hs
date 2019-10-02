{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Hydra.Core.SqlDB.Language where

import           Hydra.Prelude

import qualified Hydra.Core.Domain.DB   as D
import qualified Hydra.Core.Domain.SQLDB as D

import           Language.Haskell.TH.MakeFunctor (makeFunctorInstance)

import           Database.Beam (FromBackendRow, SqlSelect)
import           Database.Beam.Backend.SQL (BeamSqlBackend)

-- runSelectReturningList
  -- :: (MonadBeam be m, BeamSqlBackend be, FromBackendRow be a)
  -- => SqlSelect be a -> m [a]

data SqlDBF be next where
  RunBeamSelect :: (BeamSqlBackend be, FromBackendRow be a) => SqlSelect be a -> (D.DBResult a -> next) -> SqlDBF be next

-- makeFunctorInstance ''SqlDBF

instance Functor (SqlDBF be) where
  fmap f (RunBeamSelect selectQ next) = RunBeamSelect selectQ (f . next)

type SqlDBL be = Free (SqlDBF be)

runBeamSelect
  :: forall be a
   . BeamSqlBackend be
  => FromBackendRow be a
  => SqlSelect be a
  -> SqlDBL be (D.DBResult a)
runBeamSelect selectQ = liftF $ RunBeamSelect selectQ id
