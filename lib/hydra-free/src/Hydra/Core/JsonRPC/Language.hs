{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Core.JsonRPC.Language where

import           Hydra.Prelude

import qualified Hydra.Domain as D
import qualified Hydra.Core.Lang.Language as L

import           Data.Aeson as A

type RpcHandler = A.Value -> D.ReqId -> L.LangL D.RpcResponse

-- | JSON RPC method
data RpcMethod next where
  RpcMethod :: D.RpcMethodTag -> RpcHandler -> (() -> next) -> RpcMethod next

instance Functor RpcMethod where
  fmap f (RpcMethod tag handler next) = RpcMethod tag handler (f . next)

-- | Rpc server description language
type RpcProtocol a = Free RpcMethod a



whenSucces
  :: ( Applicative f
     , FromJSON t
     )
  => A.Value
  -> D.ReqId
  -> (t -> f D.RpcResponse)
  -> f D.RpcResponse
whenSucces a i f = case A.fromJSON a of
  A.Success req -> f req

  -- TODO: error payload
  A.Error _ -> pure $ D.RpcResponseError (A.toJSON $ A.String "Error in parsing of args") i

makeRpcHandler
  :: ( FromJSON requestMsg
     , ToJSON responseMsg
     )
  => (requestMsg -> L.LangL responseMsg)
  -> RpcHandler
makeRpcHandler f a i = whenSucces a i $ \req -> do
  res <- f req
  pure $ D.RpcResponseResult (A.toJSON res) i

makeRpcHandler'
  :: ( FromJSON requestMsg
     , ToJSON responseMsg
     )
  => (requestMsg -> L.LangL (Either Text responseMsg))
  -> RpcHandler
makeRpcHandler' f a i = whenSucces a i $ \req -> do
  res <- f req
  pure $ case res of
    Right b -> D.RpcResponseResult (A.toJSON b)            i
    Left  t -> D.RpcResponseError  (A.toJSON $ A.String t) i



-- | Rpc method without handling errors.
-- Example of a method:
-- getBalance :: GetWalletBalance -> L.LangL WalletBalanceMsg
-- getBalance (GetWalletBalance wallet) = ...

rpcMethod
  :: ( Typeable requestMsg
     , Typeable responseMsg
     , ToJSON responseMsg
     , FromJSON requestMsg
     )
  => (requestMsg -> L.LangL responseMsg)
  -> RpcProtocol ()
rpcMethod f = liftF $ RpcMethod (D.methodToTag f) (makeRpcHandler f) id

-- | Rpc method with handling errors.
-- Error will be converted into the errorneous RpcResponse.
-- Example of a method:
-- getBalance :: GetWalletBalance -> L.LangL (Either Text WalletBalanceMsg)
-- getBalance (GetWalletBalance wallet) = ...

rpcMethodE
  :: ( Typeable requestMsg
     , Typeable responseMsg
     , ToJSON responseMsg
     , FromJSON requestMsg
     )
  => (requestMsg -> L.LangL (Either Text responseMsg))
  -> RpcProtocol ()
rpcMethodE f = liftF $ RpcMethod (D.methodToTag f) (makeRpcHandler' f) id
