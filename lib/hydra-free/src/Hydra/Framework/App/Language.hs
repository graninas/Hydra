{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hydra.Framework.App.Language where

import           Hydra.Prelude

import qualified Hydra.Core.Class                as C
import qualified Hydra.Core.Domain               as D
import qualified Hydra.Core.Language             as L

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

  CliF :: (String -> [HS.Completion])
       -> (a -> AppL D.CliAction)
       -> (String -> AppL D.CliAction)
       -> L.CliHandlerL a ()
       -> D.CliToken
       -> (() -> next)
       -> AppF next

  ServeRpc :: D.Port -> L.RpcProtocol () -> (Either D.RpcServerError () -> next) -> AppF next


instance Functor AppF where
  fmap g (EvalProcess p next)                     = EvalProcess p                     (g . next)
  fmap g (EvalLang act next)                      = EvalLang act                      (g . next)
  fmap g (InitKVDB cfg name next)                 = InitKVDB cfg name                 (g . next)
  fmap g (InitSqlDB cfg next)                     = InitSqlDB cfg                     (g . next)
  fmap g (ServeRpc port protocol next)            = ServeRpc port protocol            (g . next)
  fmap g (CliF completeFunc onStep onUnknownCommand handlers cliToken next)
    = CliF completeFunc onStep onUnknownCommand handlers cliToken (g . next)

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

-- | Do not make it evaluating many times.
cliF
  :: (String -> [HS.Completion])
  -> (a -> AppL D.CliAction)
  -> (String -> AppL D.CliAction)
  -> L.CliHandlerL a ()
  -> AppL D.CliToken
cliF completionFunc onStep onUnknownCommand handlers = do
  token <- D.CliToken <$> (scenario $ L.newVarIO False)
  liftF $ CliF completionFunc onStep onUnknownCommand handlers token id
  pure token

cli
  :: (a -> AppL D.CliAction)
  -> (String -> AppL D.CliAction)
  -> L.CliHandlerL a ()
  -> AppL D.CliToken
cli = cliF (\_ -> [])

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

instance C.IOL AppL where
  evalIO = evalLang' . C.evalIO

-- | State handling.
-- Note: don't spawn variables uncontrollably.
-- Variables cannot be deleted.
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

serveRpc :: D.Port -> L.RpcProtocol () -> AppL (Either D.RpcServerError ())
serveRpc port protocol = liftF $ ServeRpc port protocol id
