
module Hydra.Core.CliHandlers.Language
  ( CliHandlerF (..)
  , CliHandlerL
  , cmd
  ) where

import           Hydra.Prelude

import qualified Hydra.Core.Lang.Language as L

data CliHandlerF a next where
  Cmd :: String -> L.LangL a -> (() -> next) -> CliHandlerF a next

instance Functor (CliHandlerF a) where
  fmap g (Cmd cmdStr method next) = Cmd cmdStr method (g . next)

type CliHandlerL a next = Free (CliHandlerF a) next


cmd
  :: String
  -> L.LangL a
  -> CliHandlerL a ()
cmd cmdStr handler = liftF $ Cmd cmdStr handler id
