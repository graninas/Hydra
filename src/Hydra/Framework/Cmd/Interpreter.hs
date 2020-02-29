module Hydra.Framework.Cmd.Interpreter where

import           Hydra.Prelude

import qualified Data.Map as M

import qualified Hydra.Framework.Cmd.Language as L
import qualified Hydra.Core.Runtime  as R
import qualified Hydra.Core.Interpreters  as Impl

interpretCmdHandlerL :: R.CoreRuntime -> Text -> L.CmdHandlerF a -> IO a

interpretCmdHandlerL coreRt line (L.UserCmd parser cont next) =
  next <$> case parser line of
    Nothing -> pure ()
    Just a  -> Impl.runLangL coreRt $ cont a

runCmdHandlerL :: R.CoreRuntime -> Text -> L.CmdHandlerL a -> IO a
runCmdHandlerL coreRt line = foldFree (interpretCmdHandlerL coreRt line)
