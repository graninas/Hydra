module Hydra.Core.StructuredLogger.FTLI where

import           Hydra.Prelude

import qualified Hydra.Core.FTL                  as L
import qualified Hydra.Core.RLens                as RLens
import qualified Hydra.Runtime                   as R

import qualified Data.Text                       as TXT (unpack)


-- TODO: implement it
instance MonadIO m => L.StructuredLoggerL (ReaderT R.CoreRuntime m) where
  report lvl msg mbTimestamp attrs = error "Not implemented"
