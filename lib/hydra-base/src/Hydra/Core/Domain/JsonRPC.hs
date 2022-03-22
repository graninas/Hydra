module Hydra.Core.Domain.JsonRPC where

import           Hydra.Prelude hiding ((.=))


import qualified Data.Aeson as A
import           Data.Aeson ((.:), (.=))
import           Data.Typeable
import qualified Data.Text as T



type Address = Text
type Port = Int


type RpcServerError = Text

type RpcMethodTag = Text


-- TODO: it's completely unclear where this ReqId comes from in Enecuum
type ReqId = Int

data RpcRequest = RpcRequest Text A.Value ReqId
  deriving (Show)

data RpcResponse
  = RpcResponseResult A.Value ReqId
  | RpcResponseError A.Value ReqId
  deriving (Show)


methodToTag :: Typeable a => a -> Text
methodToTag = T.pack . takeWhile (/= ' ') . show . typeOf


-- toRpcRequest :: (Typeable a, ToJSON a) => a -> RpcRequest
-- toRpcRequest a = RpcRequest (T.pack . show . typeOf $ a) (toJSON a) 0

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
