{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hydra.Core.Lang.Language where

import           Hydra.Prelude

import qualified Hydra.Core.ControlFlow.Class    as L
import qualified Hydra.Core.ControlFlow.Language as L
import qualified Hydra.Core.Logger.Class         as L
import qualified Hydra.Core.Logger.Language      as L
import qualified Hydra.Core.Random.Class         as L
import qualified Hydra.Core.Random.Language      as L
import qualified Hydra.Core.State.Class          as L
import qualified Hydra.Core.State.Language       as L
import qualified Hydra.Core.KVDB.Language        as L
import qualified Hydra.Core.SqlDB.Language       as L
import qualified Hydra.Core.Lang.Class           as C
import qualified Hydra.Core.Domain               as D

import           Language.Haskell.TH.MakeFunctor (makeFunctorInstance)
import           Database.Beam.Backend.SQL (BeamSqlBackend)
import           Database.Beam (FromBackendRow, SqlSelect)
import           Database.Beam.Sqlite (Sqlite)
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as B

-- | Core effects container language.
data LangF next where
  -- | Eval stateful action atomically.
  EvalStateAtomically :: L.StateL a -> (a -> next) -> LangF next
  -- | Logger effect
  EvalLogger      :: L.LoggerL ()     -> (() -> next) -> LangF next
  -- | Random effect
  EvalRandom      :: L.RandomL a     -> (a  -> next) -> LangF next
  -- | ControlFlow effect
  EvalControlFlow :: L.ControlFlowL a -> (a  -> next) -> LangF next
  -- | Impure effect. Avoid using it in production code (it's not testable).
  EvalIO          :: IO a           -> (a  -> next) -> LangF next
  -- | Eval KV DB action
  EvalKVDB :: D.DB db => D.DBHandle db -> L.KVDBL db a -> (a -> next) -> LangF next
  -- | Eval SQL DB
  EvalSqlDB :: D.SqlConn beM -> L.SqlDBL beM a -> (D.DBResult a -> next) -> LangF next



makeFunctorInstance ''LangF

type LangL = Free LangF

class IOL m where
  evalIO :: IO a -> m a

instance IOL LangL where
  evalIO io = liftF $ EvalIO io id

evalStateAtomically' :: L.StateL a -> LangL a
evalStateAtomically' action = liftF $ EvalStateAtomically action id

evalLogger' :: L.LoggerL () -> LangL ()
evalLogger' logger = liftF $ EvalLogger logger id

evalRandom' :: L.RandomL a -> LangL a
evalRandom' g = liftF $ EvalRandom g id

evalControlFlow' :: L.ControlFlowL a -> LangL a
evalControlFlow' a = liftF $ EvalControlFlow a id

instance C.Lang L.LoggerL L.RandomL L.ControlFlowL L.StateL LangL where
  evalStateAtomically = evalStateAtomically'
  evalLogger          = evalLogger'
  evalRandom          = evalRandom'
  evalControlFlow     = evalControlFlow'

instance L.StateIO LangL where
  newVarIO       = evalStateAtomically' . L.newVar
  readVarIO      = evalStateAtomically' . L.readVar
  writeVarIO var = evalStateAtomically' . L.writeVar var
  retryIO        = evalStateAtomically' L.retry

instance L.Atomically L.StateL LangL where
  atomically = evalStateAtomically'

instance L.Logger LangL where
  logMessage level msg = evalLogger' $ L.logMessage level msg

instance L.Random LangL where
  getRandomInt = evalRandom' . L.getRandomInt

instance L.ControlFlow LangL where
  delay i = evalControlFlow' $ L.delay i


evalKVDB :: forall db a. D.DB db => D.DBHandle db -> L.KVDBL db a -> LangL a
evalKVDB handle script = liftF $ EvalKVDB handle script id

withKVDB :: forall db a. D.DB db => D.DBHandle db -> L.KVDBL db a -> LangL a
withKVDB = evalKVDB

evalSqlDB
  ::
    ( D.BeamRunner beM
    , D.BeamRuntime be beM
    )
  => D.SqlConn beM
  -> L.SqlDBL beM a
  -> LangL (D.DBResult a)
evalSqlDB conn dbAct = liftFC $ EvalSqlDB conn dbAct id

runDB
  ::
    ( D.BeamRunner beM
    , D.BeamRuntime be beM
    )
  => D.SqlConn beM
  -> L.SqlDBL beM a
  -> LangL (D.DBResult a)
runDB = evalSqlDB
