module Hydra.Framework.Cmd.Interpreter where

import           Hydra.Prelude

import qualified Data.Map as M

import qualified Hydra.Framework.Cmd.Language as L

-- TODO: rework.

interpretCmdHandlerL :: MVar (M.Map Text L.CmdHandler) -> L.CmdHandlerF a -> IO a
interpretCmdHandlerL methodsMVar (L.CmdHandler name method' next) = do
  methods <- takeMVar methodsMVar
  putMVar methodsMVar $ M.insert name method' methods
  pure $ next ()

runCmdHandlerL :: MVar (Map Text L.CmdHandler) -> L.CmdHandlerL a -> IO a
runCmdHandlerL m = foldFree (interpretCmdHandlerL m)
