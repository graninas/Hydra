module Hydra.Core.JsonRPC.Interpreter where

import           Hydra.Prelude

import qualified Hydra.Core.Domain.JsonRPC as D
import qualified Hydra.Core.JsonRPC.Language as L
import qualified Hydra.Runtime             as R

import qualified Data.Map as Map
import           Control.Concurrent (forkFinally)
import qualified Control.Exception.Safe as Safe
import qualified Network as N
import qualified Network.Socket as S hiding (recv)
import qualified Network.Socket.ByteString.Lazy as S



interpretRpcHandlerL :: TVar (M.Map R.HandlerName (R.RpcHandler m)) -> L.RpcMethod m a -> IO a
interpretRpcHandlerL handlersRef (R.RpcHandler name method next) = do
  modifyIORef' handlersRef $ Map.insert name method
  pure $ next ()

runRpcProtocol :: IORef (Map R.HandlerName (R.RpcHandler m)) -> L.RpcProtocol m a -> IO a
runRpcProtocol handlersRef protocol = foldFree (interpretRpcHandlerL handlersRef protocol)


prepareRpcHandlers :: IORef (Map Text (RpcHandler m)) -> RpcProtocol m a -> IO a
prepareRpcHandlers = runRpcProtocol
