module Hydra.Testing.Functional.TestRuntime where

import           Hydra.Prelude

import qualified Data.Map              as Map

import qualified Hydra.Core.Domain     as D
import qualified Hydra.Core.Language   as L
import qualified Hydra.Runtime         as R
import qualified Hydra.Interpreters    as R

data Step
  = Mock Any
  | RunTestInterpreter
  | RunRealInterpreter


data TestRuntime = TestRuntime
  { _traceSteps :: Bool
  , _appRuntime :: Maybe R.AppRuntime
  , _steps      :: IORef [Step]
  }


popNextStep :: TestRuntime -> IO (Maybe Step)
popNextStep (TestRuntime {_steps}) = do
  lst <- readIORef _steps
  case lst of
    []     -> pure Nothing
    (m:ms) -> writeIORef _steps ms >> pure (Just m)
