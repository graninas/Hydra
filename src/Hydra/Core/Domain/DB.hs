{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Hydra.Core.Domain.DB where

import           Hydra.Prelude

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS

data DBErrorType
    = SystemError
    | KeyNotFound
    | InvalidType
    | DecodingFailed
    deriving (Generic, Ord, Eq, Enum, Bounded, Show, Read)

data DBError = DBError DBErrorType Text
    deriving (Generic, Ord, Eq, Show, Read)

type DBResult a = Either DBError a

data KVDBStorage db = KVDBStorage FilePath
    deriving (Show, Generic)
