{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Hydra.Core.Domain.DB where

import           Hydra.Prelude
import           Data.Aeson.Extra     (noLensPrefix)
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS

data DBErrorType
    = SystemError
    | KeyNotFound
    | InvalidType
    deriving (Generic, Ord, Eq, Enum, Bounded, Show, Read)

data DBError = DBError DBErrorType Text
    deriving (Generic, Ord, Eq, Show, Read)

type DBResult a = Either DBError a

data KVDBConn db = KVDBConn
    deriving (Show, Generic)
