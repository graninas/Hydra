module Labyrinth.Prelude
  ( module X
  , putStrLn
  ) where

import           Hydra.Prelude as X hiding (retry, atomically, putStrLn)
import qualified Hydra.Prelude as PP
import qualified Prelude as P
import qualified Data.Text as T

import qualified Hydra.Domain               as D
import qualified Hydra.Language             as L
import           Hydra.Language             as X
import           Hydra.Domain               as X

putTextLn :: Text -> L.LangL ()
putTextLn = L.evalIO . PP.putStrLn

putStrLn :: String -> L.LangL ()
putStrLn = L.evalIO . PP.putStrLn . T.pack
