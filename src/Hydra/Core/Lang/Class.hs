{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Hydra.Core.Lang.Class where

import           Hydra.Prelude

import qualified Hydra.Core.ControlFlow.Class    as C
import qualified Hydra.Core.Domain               as D
import qualified Hydra.Core.Logger.Class         as C
import qualified Hydra.Core.Random.Class         as C
import qualified Hydra.Core.State.Class          as C

import           Language.Haskell.TH.MakeFunctor (makeFunctorInstance)

-- TODO: this is awful
class (C.Logger l, C.Random r, C.ControlFlow cf, C.State' s, Monad m)
  => Lang l r cf s m
  | m -> l, m -> r, m -> cf, m -> s where
  evalLogger          :: l () -> m ()
  evalRandom          :: r a  -> m a
  evalStateAtomically :: s a  -> m a
  evalControlFlow     :: cf a -> m a
  -- todo: io


-- TODO: this should not be here.
class IOL m where
  evalIO :: IO a -> m a
