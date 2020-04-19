{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hydra.Framework.App.Language where

import           Hydra.Prelude

import qualified Hydra.Core.Class                as C
import qualified Hydra.Core.Domain               as D
import qualified Hydra.Core.Language             as L
import qualified Hydra.Framework.Cmd.Language    as L

import           Language.Haskell.TH.MakeFunctor (makeFunctorInstance)
import           Database.Beam.Sqlite (Sqlite)
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B
import qualified System.Console.Haskeline as HS

-- | App language.
data AppF next where
  -- | Eval process.
  EvalProcess :: L.ProcessL L.LangL a -> (a -> next) -> AppF next
  -- | Eval lang.
  EvalLang :: L.LangL a -> (a -> next) -> AppF next

  -- | Init KV DB.
  -- A new connection will be created and stored.
  -- No need to explicitly close the connections.
  -- They will be closed automatically on the program finish.
  InitKVDB :: D.DB db => D.KVDBConfig db -> D.DBName -> (D.DBResult (D.DBHandle db) -> next) -> AppF next
  -- TODO: add explicit deinit.
  -- DeinitKVDB :: D.DB db => D.DBHandle db -> (D.DBResult Bool -> next) -> AppF next

  -- | Init SQL DB connection.
  -- If connection exists, `DBError ConnectionAlreadyExists "..."` will be returned.
  InitSqlDB :: D.DBConfig beM -> (D.DBResult (D.SqlConn beM) -> next) -> AppF next

  -- DeInitSqlDB
  --   :: T.SqlConn beM
  --   -> (() -> next)
  --   -> FlowMethod next

  StdF :: (String -> [HS.Completion]) -> L.CmdHandlerL () -> (() -> next) -> AppF  next

  TeaF :: (String -> [HS.Completion])
       -> (a -> AppL D.TeaAction)
       -> (String -> AppL D.TeaAction)
       -> L.TeaHandlerL a ()
       -> D.TeaToken
       -> (() -> next)
       -> AppF  next


instance Functor AppF where
  fmap g (EvalProcess p next)                     = EvalProcess p                     (g . next)
  fmap g (EvalLang act next)                      = EvalLang act                      (g . next)
  fmap g (InitKVDB cfg name next)                 = InitKVDB cfg name                 (g . next)
  fmap g (InitSqlDB cfg next)                     = InitSqlDB cfg                     (g . next)
  fmap g (StdF completeFunc handlers next)        = StdF completeFunc handlers        (g . next)
  fmap g (TeaF completeFunc onStep onUnknownCommand handlers teaToken next)
    = TeaF completeFunc onStep onUnknownCommand handlers teaToken (g . next)

type AppL = Free AppF

-- | Eval lang.
evalLang' :: L.LangL a -> AppL a
evalLang' action = liftF $ EvalLang action id

-- | Eval lang.
scenario :: L.LangL a -> AppL a
scenario = evalLang'

-- | Eval process.
evalProcess' :: L.ProcessL L.LangL a -> AppL a
evalProcess' action = liftF $ EvalProcess action id

std :: L.CmdHandlerL () -> AppL ()
std handlers = liftF $ StdF (\_ -> []) handlers id

stdF :: (String -> [HS.Completion]) -> L.CmdHandlerL () -> AppL ()
stdF completionFunc handlers = liftF $ StdF completionFunc handlers id

-- | Do not make it evaluating many times.
teaF
  :: (String -> [HS.Completion])
  -> (a -> AppL D.TeaAction)
  -> (String -> AppL D.TeaAction)
  -> L.TeaHandlerL a ()
  -> AppL D.TeaToken
teaF completionFunc onStep onUnknownCommand handlers = do
  token <- D.TeaToken <$> (scenario $ L.newVarIO False)
  liftF $ TeaF completionFunc onStep onUnknownCommand handlers token id
  pure token

tea
  :: (a -> AppL D.TeaAction)
  -> (String -> AppL D.TeaAction)
  -> L.TeaHandlerL a ()
  -> AppL D.TeaToken
tea = teaF (\_ -> [])

instance C.Process L.LangL AppL where
  forkProcess  = evalProcess' . L.forkProcess'
  killProcess  = evalProcess' . L.killProcess'
  tryGetResult = evalProcess' . L.tryGetResult'
  awaitResult  = evalProcess' . L.awaitResult'

-- | Fork a process and keep the Process Ptr.
fork :: L.LangL a -> AppL (D.ProcessPtr a)
fork = evalProcess' . L.forkProcess'

-- | Fork a process and forget.
process :: L.LangL a -> AppL ()
process action = void $ fork action

instance L.IOL AppL where
  evalIO = evalLang' . L.evalIO

instance L.StateIO AppL where
  newVarIO       = evalLang' . L.newVarIO
  readVarIO      = evalLang' . L.readVarIO
  writeVarIO var = evalLang' . L.writeVarIO var
  retryIO        = evalLang' L.retryIO

instance L.Atomically L.StateL AppL where
  atomically = evalLang' . L.atomically

instance L.Logger AppL where
  logMessage level msg = evalLang' $ L.logMessage level msg

instance L.Random AppL where
  getRandomInt = evalLang' . L.getRandomInt

instance L.ControlFlow AppL where
  delay = evalLang' . L.delay

initKVDB :: forall db. D.DB db => D.KVDBConfig db -> AppL (D.DBResult (D.DBHandle db))
initKVDB config = do
  let dbName = D.getDBName @db
  liftF $ InitKVDB config dbName id

initSqlDB :: D.DBConfig beM -> AppL (D.DBResult (D.SqlConn beM))
initSqlDB cfg = liftF $ InitSqlDB cfg id

-- deinitSqlDB :: T.SqlConn beM -> Flow ()
-- deinitSqlDB conn = liftFC $ DeInitSqlDBConnection conn id
