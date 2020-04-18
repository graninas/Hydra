{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}

module Hydra.Framework.Tea.Language
  ( TeaHandlerF (..)
  , TeaHandlerL
  , cmd
  ) where

import           Hydra.Prelude
import qualified Data.Text          as T

import qualified Hydra.Core.Language as L

data TeaHandlerF a next where
  Cmd :: String -> L.LangL a -> (() -> next) -> TeaHandlerF a next

instance Functor (TeaHandlerF a) where
  fmap g (Cmd cmdStr method next) = Cmd cmdStr method (g . next)

type TeaHandlerL a next = Free (TeaHandlerF a) next


cmd
  :: String
  -> L.LangL a
  -> TeaHandlerL a ()
cmd cmdStr handler = liftF $ Cmd cmdStr handler id
