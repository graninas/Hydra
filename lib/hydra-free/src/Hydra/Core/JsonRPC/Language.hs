{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.JsonRPC.Language where

import           Hydra.Prelude

import qualified Hydra.Domain as D
import qualified Hydra.Core.Lang.Language as L


-- | JSON RPC server
data RpcMethod next where
  RpcMethod :: (rpcRequest -> L.LangL rpcResponse) -> (() -> next ) -> RpcMethod next

instance Functor RpcMethodF where
  fmap f (RpcMethod methodF next) = RpcMethod methodF (f . next)


type RpcProtocol a = F RpcMethod a


rpcMethod
  :: (rpcRequest -> L.LangL rpcResponse)
  -> RpcProtocol ()
rpcMethod methodF = liftFC $ RpcMethod methodF id
