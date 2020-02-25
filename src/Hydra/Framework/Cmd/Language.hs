{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}

-- TODO: rework.
module Hydra.Framework.Cmd.Language
  ( CmdHandlerF (..)
  , CmdHandler
  , CmdHandlerL
  , stdHandler
  , toTag
  ) where

import           Hydra.Prelude
import qualified Data.Text          as T
import           Data.Typeable

import qualified Hydra.Core.Language as L

toTag :: Typeable a => a -> Text
toTag = T.pack . takeWhile (/= ' ') . show . typeOf

data CmdHandlerF a where
    CmdHandler :: Text -> CmdHandler -> (() -> a)  -> CmdHandlerF a

instance Functor CmdHandlerF where
    fmap g (CmdHandler text f next) = CmdHandler text f (g . next)

type CmdHandler    = String -> L.LangL Text
type CmdHandlerL a = Free CmdHandlerF a

stdHandler :: (Typeable a, Read a) => (a -> L.LangL Text) -> CmdHandlerL ()
stdHandler f = liftF $ CmdHandler (toTag f) (makeStdHandler f) id
  where
    makeStdHandler :: Read a => (a -> L.LangL Text) -> String -> L.LangL Text
    makeStdHandler f raw = case readMaybe raw of
      Just req -> f req
      Nothing  -> pure "Error of request parsing"
