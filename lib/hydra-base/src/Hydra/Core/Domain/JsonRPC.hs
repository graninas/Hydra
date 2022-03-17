module Hydra.Domain.JsonRPC where

import           Hydra.Prelude


import qualified Data.Aeson as A
import           Data.Typeable



type Address = Text
type Port = Int


type RpcServerError = Text
