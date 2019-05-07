{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Hydra.Core.Evaluable where

import           Hydra.Prelude

import qualified Hydra.Core.Runtime as R

class Evaluable m where
  -- type EvalM m :: * -> *
  -- evaluate :: EvalM m a -> R.CoreRuntime -> IO a
  evaluate :: m a -> R.CoreRuntime -> IO a
