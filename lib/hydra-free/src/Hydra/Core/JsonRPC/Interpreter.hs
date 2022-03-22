module Hydra.Core.JsonRPC.Interpreter where

import           Hydra.Prelude

import qualified Hydra.Core.Domain.JsonRPC as D
import qualified Hydra.Core.JsonRPC.Language as L

import qualified Data.Map as Map


type RpcHandlers = Map D.RpcMethodTag L.RpcHandler

interpretRpcMethod :: IORef RpcHandlers -> L.RpcMethod a -> IO a
interpretRpcMethod handlersRef (L.RpcMethod tag handler next) = do
  modifyIORef' handlersRef $ Map.insert tag handler
  pure $ next ()

runRpcProtocol :: IORef RpcHandlers -> L.RpcProtocol a -> IO a
runRpcProtocol handlersRef = foldFree (interpretRpcMethod handlersRef)

prepareRpcHandlers :: IORef RpcHandlers -> L.RpcProtocol a -> IO a
prepareRpcHandlers = runRpcProtocol
