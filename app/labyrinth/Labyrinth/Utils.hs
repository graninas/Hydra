module Labyrinth.Utils where

import           Hydra.Prelude

import qualified Hydra.Domain               as D
import qualified Hydra.Language             as L

putStrLn :: Text -> L.LangL ()
putStrLn = L.evalIO . P.putStrLn
