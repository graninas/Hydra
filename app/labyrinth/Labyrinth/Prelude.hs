module Labyrinth.Prelude
  ( module X
  , putStrLn
  ) where

import           Hydra.Prelude as X hiding (retry, atomically, putStrLn)
import qualified Hydra.Prelude as PP
import qualified Prelude as P

import qualified Hydra.Domain               as D
import qualified Hydra.Language             as L
import           Hydra.Language             as X
import           Hydra.Domain               as X

putStrLn :: Text -> L.LangL ()
putStrLn = L.evalIO . PP.putStrLn
