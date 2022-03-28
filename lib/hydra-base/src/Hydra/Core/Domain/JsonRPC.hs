{-# LANGUAGE FunctionalDependencies #-}

module Hydra.Core.Domain.JsonRPC where

import           Hydra.Prelude hiding ((.=))

import qualified Data.Aeson as A
import           Data.Aeson ((.:), (.=))
import           Data.Typeable
import qualified Data.Text as T

import qualified Hydra.Core.Domain.Networking as D

type RpcServerError = Text

type RpcMethodTag = Text

type ReqId = Int

data RpcRequest = RpcRequest Text A.Value ReqId
  deriving (Show)

data RpcResponse
  = RpcResponseResult A.Value ReqId
  | RpcResponseError A.Value ReqId
  deriving (Show)


methodToTag :: Typeable a => a -> Text
methodToTag = T.pack . takeWhile (/= ' ') . show . typeOf

class
  ( Typeable req, ToJSON req, FromJSON resp )
  => Rpc req resp | req -> resp, resp -> req where
  toRpcRequest
    :: req
    -> RpcRequest
  toRpcRequest a = RpcRequest (T.pack . show . typeOf $ a) (toJSON a) 0

  fromRpcResponse
    :: RpcResponse
    -> Either D.NetworkError resp
  fromRpcResponse resp = case resp of
    RpcResponseError (A.String err) _ -> Left err
    RpcResponseError err _ -> Left $ show err
    RpcResponseResult val _ ->
      case A.fromJSON val of
        A.Error   txt  -> Left $ T.pack txt
        A.Success resp' -> Right resp'


instance FromJSON RpcRequest where
  parseJSON (A.Object o) = RpcRequest
    <$> o .: "method"
    <*> o .: "params"
    <*> o .: "id"
  parseJSON _ = error "Unknown value to parse."

instance ToJSON RpcRequest where
  toJSON (RpcRequest method val requesId) = A.object
    [ "method" .= method
    , "params" .= val
    , "id"     .= requesId
    ]

instance ToJSON RpcResponse where
  toJSON (RpcResponseResult val requesId) = A.object
    [ "result" .= val
    , "id"     .= requesId
    ]
  toJSON (RpcResponseError val requesId) = A.object
    [ "error"  .= val
    , "id"     .= requesId
    ]

instance FromJSON RpcResponse where
  parseJSON (A.Object a)
    = (RpcResponseResult  <$> a .: "result" <*> a .: "id")
    <|> (RpcResponseError <$> a .: "error" <*> a .: "id")
  parseJSON _ = error "Unknown value to parse."
