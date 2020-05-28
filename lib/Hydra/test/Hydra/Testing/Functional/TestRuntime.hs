module Hydra.Testing.Functional.TestRuntime where

import           Hydra.Prelude

import qualified Data.Map                        as Map

import qualified Hydra.Core.Domain               as D
import qualified Hydra.Core.Language             as L

data Mock
  = Mock Any
  | RunTestInterpreter
  | RunRealInterpreter

-- | Runtime data for core subsystems.
data TestRuntime = TestRuntime
    { evalLangMocks :: IORef [Mock]
    }


popNextMock :: IORef [Mock] -> Maybe Mock
popNextMock ref = do
  lst <- readIORef ref
  case lst of
    [] -> pure Nothing
    (m:ms) -> writeIORef ref ms >> pure $ Just m
