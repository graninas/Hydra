module Hydra.Testing.Functional.TestRuntime where

import           Hydra.Prelude

import qualified Data.Map                        as Map

import qualified Hydra.Core.Domain               as D
import qualified Hydra.Core.Language             as L

data Mock
  = Mock Any
  | RunTestInterpreter
  | RunRealInterpreter


data TestRuntime = TestRuntime
    { mocks :: IORef [Mock]
    }


popNextMock :: TestRuntime -> IO (Maybe Mock)
popNextMock (TestRuntime {mocks}) = do
  lst <- readIORef mocks
  case lst of
    []     -> pure Nothing
    (m:ms) -> writeIORef mocks ms >> pure (Just m)
