module Hydra.Testing.Functional.Common where

import           Hydra.Prelude

import           Unsafe.Coerce (unsafeCoerce)

import qualified Hydra.Core.Domain        as D
import qualified Hydra.Core.Language      as L
import qualified Hydra.Framework.Language as L
import qualified Hydra.Runtime            as R
import qualified Hydra.Interpreters       as R

import           Hydra.Testing.Functional.TestRuntime
import qualified Hydra.Testing.Functional.RLens as RLens

type TestAct a = IO a

withStep
  :: String
  -> TestRuntime
  -> (t -> a)
  -> TestAct t
  -> (R.AppRuntime -> rt, rt -> IO a)
  -> IO a
withStep stepName testRt next testAct (realRtF, realAct) = do
  when (testRt ^. RLens.traceSteps) $ putStrLn stepName
  mbMock <- popNextStep testRt
  let mbRealAppRt = testRt ^. RLens.appRuntime
  case (mbMock, mbRealAppRt) of
    (Just (Mock ghcAny)     , _)          -> pure $ next $ unsafeCoerce ghcAny
    (Just RunTestInterpreter, _)          -> next <$> testAct
    (Just RunRealInterpreter, Nothing)    -> error $ show stepName <> ": Real runtime is not ready."
    (Just RunRealInterpreter, Just appRt) -> realAct $ realRtF appRt
    (Nothing                , _)          -> error $ show stepName <> ": Mock not found."


initKVDB' :: forall db. D.DB db => TestRuntime -> D.KVDBConfig db -> String -> IO (D.DBResult (D.DBHandle db))
initKVDB' _ _ dbName = pure $ Right $ D.DBHandle D.MockedKVDB dbName
