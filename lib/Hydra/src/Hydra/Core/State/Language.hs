{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.State.Language where

import           Hydra.Prelude

import qualified Hydra.Core.Domain               as D
import qualified Hydra.Core.Logger.Class         as L
import qualified Hydra.Core.Logger.Language      as L
import qualified Hydra.Core.State.Class          as L

-- | State language. It reflects STM and its behavior.
data StateF next where
  -- | Create variable.
  NewVar :: a -> (D.StateVar a -> next) -> StateF next
  -- | Read variable.
  ReadVar :: D.StateVar a -> (a -> next) -> StateF next
  -- | Write variable.
  WriteVar :: D.StateVar a -> a -> (() -> next) -> StateF next
  -- | Retry until some variable is changed in this atomic block.
  Retry :: (a -> next) -> StateF next
  -- | Eval "delayed" logger: it will be written after successfull state operation.
  EvalStmLogger :: L.LoggerL () -> (() -> next) -> StateF next

instance Functor StateF where
  fmap f (NewVar        val next)     = NewVar        val     (f . next)
  fmap f (ReadVar       var next)     = ReadVar       var     (f . next)
  fmap f (WriteVar      var val next) = WriteVar      var val (f . next)
  fmap f (Retry         next)         = Retry                 (f . next)
  fmap f (EvalStmLogger logActt next) = EvalStmLogger logActt (f . next)


type StateL = Free StateF

-- | State handling.
-- Note: don't spawn variables uncontrollably.
-- Variables cannot be deleted.

instance L.State' StateL where
  newVar   val     = liftF $ NewVar val id
  readVar  var     = liftF $ ReadVar var id
  writeVar var val = liftF $ WriteVar var val id
  retry            = liftF $ Retry id

instance L.Logger StateL where
  logMessage level msg = liftF $ EvalStmLogger (L.logMessage level msg) id
