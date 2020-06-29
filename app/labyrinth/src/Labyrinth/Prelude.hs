{-|
under lib Hydra src prelude file ... 
-}

module Labyrinth.Prelude
  ( module X
  , putStrLn
  , putTextLn
  ) where

import           Hydra.Prelude as X hiding (retry, atomically, putStrLn, putTextLn)
import qualified Hydra.Prelude as PP
import qualified Data.Text as T

import qualified Hydra.Language             as L
import           Hydra.Language             as X
import           Hydra.Domain               as X

putTextLn :: Text -> L.LangL ()
putTextLn = L.evalIO . PP.putStrLn

putStrLn :: String -> L.LangL ()
putStrLn = putTextLn . T.pack
