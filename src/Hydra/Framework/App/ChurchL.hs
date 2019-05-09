{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Framework.App.ChurchL where

import           Hydra.Prelude

import qualified Hydra.Core.Domain            as D
import qualified Hydra.Core.Language          as L
import qualified Hydra.Framework.App.Language as L

type AppL = F L.AppF

-- | Eval lang.
evalLang :: L.LangL a -> AppL a
evalLang action = liftF $ L.EvalLang action id

-- | Eval lang.
scenario :: L.LangL a -> AppL a
scenario = evalLang
--
-- -- | Eval process.
-- evalProcess :: L.ProcessL L.LangL a -> AppL a
-- evalProcess action = liftF $ EvalProcess action id
--
-- -- | Fork a process and keep the Process Ptr.
-- fork :: L.LangL a -> AppL (D.ProcessPtr a)
-- fork action = evalProcess (L.forkProcess action)
--
-- -- | Fork a process and forget.
-- process :: L.LangL a -> AppL ()
-- process action = void $ fork action
--
-- instance L.IOL AppL where
--   evalIO = evalLang . L.evalIO
--
-- instance L.StateIO AppL where
--     newVarIO       = evalLang . L.newVarIO
--     readVarIO      = evalLang . L.readVarIO
--     writeVarIO var = evalLang . L.writeVarIO var
--
-- instance L.Logger AppL where
--   logMessage level msg = evalLang $ L.logMessage level msg
--
-- instance L.Random AppL where
--   getRandomInt = evalLang . L.getRandomInt

instance L.ControlFlow AppL where
  delay = evalLang . L.delay
