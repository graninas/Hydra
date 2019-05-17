{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hydra.Framework.App.Class where

import           Hydra.Prelude

import qualified Hydra.Core.Class                as C
import qualified Hydra.Core.Domain               as D

import           Language.Haskell.TH.MakeFunctor (makeFunctorInstance)

class (C.Process lang proc, Monad m) => App m where
  evalLang    :: lang a -> m a
  evalProcess :: proc a -> m a
