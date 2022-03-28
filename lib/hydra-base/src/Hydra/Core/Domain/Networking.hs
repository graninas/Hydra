module Hydra.Core.Domain.Networking where

import           Hydra.Prelude


type Host = Text
type Port = Int

data Address = Address Host Port
  deriving (Show, Eq, Ord)

type NetworkError = Text
