module Hydra.Framework.App.Interpreter where

import           Hydra.Prelude

import qualified Data.Map as Map
import qualified Data.Text as T

import qualified Hydra.Core.Domain        as D
import qualified Hydra.Core.Interpreters  as Impl
import qualified Hydra.Core.Language      as L
import qualified Hydra.Core.RLens         as RLens
import qualified Hydra.Core.Runtime       as R
import qualified Hydra.Core.KVDBRuntime   as R
import qualified Hydra.Core.SqlDBRuntime  as R
import qualified Hydra.Framework.Language as L
import qualified Hydra.Framework.RLens    as RLens
import qualified Hydra.Framework.Runtime  as R
import qualified Hydra.Framework.Cmd.Interpreter as Impl

import qualified Database.RocksDB         as Rocks
import qualified Database.Redis           as Redis
import qualified Database.SQLite.Simple   as SQLite
import           Database.Beam.Sqlite     (Sqlite)
import qualified System.Console.Haskeline         as HS
import qualified System.Console.Haskeline.History as HS

langRunner :: R.CoreRuntime -> Impl.LangRunner L.LangL
langRunner coreRt = Impl.LangRunner (Impl.runLangL coreRt)

initKVDB' :: forall db. D.DB db => R.CoreRuntime -> D.KVDBConfig db -> String -> IO (D.DBResult (D.DBHandle db))
initKVDB' coreRt cfg@(D.RocksDBConfig _ _ _) dbName =
  R.initRocksDB' (coreRt ^. RLens.rocksDBs) cfg dbName
initKVDB' coreRt cfg@(D.RedisConfig) dbName =
  R.initRedisDB' (coreRt ^. RLens.redisConns) cfg dbName

connect :: D.DBConfig beM -> IO (D.DBResult (D.SqlConn beM))
connect cfg = do
  eConn <- try $ R.connect' cfg
  case eConn of
    Left (e :: SomeException) -> pure $ Left $ D.DBError D.FailedToConnect $ show e
    Right conn -> pure $ Right conn

evalTeaAction :: R.CoreRuntime -> D.TeaToken -> D.TeaAction -> HS.InputT IO Bool
evalTeaAction coreRt teaToken (D.TeaFinish mbMsg) = do
      whenJust mbMsg HS.outputStrLn
      liftIO $ Impl.runLangL coreRt $ L.writeVarIO (D.teaFinishedToken teaToken) True
      pure True
evalTeaAction _ teaToken D.TeaLoop            = pure True
evalTeaAction _ teaToken (D.TeaOutputMsg msg) = HS.outputStrLn msg >> pure True

interpretAppF :: R.AppRuntime -> L.AppF a -> IO a
interpretAppF appRt (L.EvalLang action next) = do
  let coreRt = appRt ^. RLens.coreRuntime
  res <- Impl.runLangL coreRt action
  pure $ next res

interpretAppF appRt (L.EvalProcess action next) = do
  let coreRt = appRt ^. RLens.coreRuntime
  res <- Impl.runProcessL (langRunner coreRt) (coreRt ^. RLens.processRuntime) action
  pure $ next res

interpretAppF appRt (L.InitKVDB cfg dbName next) =
  next <$> initKVDB' (appRt ^. RLens.coreRuntime) cfg dbName

interpretAppF appRt (L.InitSqlDB cfg next) = do
  let connTag = D.getConnTag cfg
  let connsVar = appRt ^. RLens.coreRuntime . RLens.sqlConns
  connMap <- takeMVar connsVar
  case Map.lookup connTag connMap of
    Just _ -> do
      putMVar connsVar connMap
      pure
        $ next $ Left $ D.DBError D.ConnectionAlreadyExists
        $ "Connection for " <> show connTag <> " already created."
    Nothing -> do
      eConn <- connect cfg
      case eConn of
        Right conn -> do
          putMVar connsVar $ Map.insert connTag (R.bemToNative conn) connMap
          pure $ next $ Right conn
        Left err -> do
          putMVar connsVar connMap
          pure $ next $ Left err


interpretAppF appRt (L.StdF completeFunc stdDef next) = do
  let coreRt = appRt ^. RLens.coreRuntime
  msgsRef    <- newIORef []
  successRef <- newIORef False
  -- TODO: add history.
  void $ forkIO $ do
    let loop = HS.getInputLine "> " >>= \case
          Nothing   -> pure ()
          Just line -> do
            liftIO $ writeIORef msgsRef []
            liftIO $ writeIORef successRef False
            liftIO $ Impl.runCmdHandlerL coreRt msgsRef successRef line stdDef
            msgs    <- readIORef msgsRef
            success <- readIORef successRef
            mapM_ HS.outputStrLn $ reverse msgs
            when (null msgs && not success) $ HS.outputStrLn "Unknown command."
            loop
    let cf = HS.completeWord Nothing " \t" $ pure . completeFunc
    HS.runInputT (HS.setComplete cf HS.defaultSettings) loop
  pure $ next ()


interpretAppF appRt (L.TeaF completeFunc onStep onUnknownCommand handlers teaToken next) = do
  let coreRt = appRt ^. RLens.coreRuntime

  handlersRef <- newIORef Map.empty
  Impl.runTeaHandlerL handlersRef handlers
  handlers <- readIORef handlersRef

  void $ forkIO $ do
    let loop = do
          mbLine <- HS.getInputLine "> "
          let eAct = case mbLine of
                Nothing -> Left Nothing
                Just line -> case Map.lookup line handlers of
                  Nothing -> Left (Just line)
                  Just act -> Right act

          doLoop <- case eAct of
            Left Nothing    -> pure True
            Left (Just cmd) -> do
              teaAction <- liftIO $ runAppL appRt $ onUnknownCommand cmd
              evalTeaAction coreRt teaToken teaAction
            Right action    -> do
              result    <- liftIO $ Impl.runLangL coreRt action
              teaAction <- liftIO $ runAppL appRt $ onStep result
              evalTeaAction coreRt teaToken teaAction

          when doLoop loop

    let cf = HS.completeWord Nothing " \t" $ pure . completeFunc
    HS.runInputT (HS.setComplete cf HS.defaultSettings) loop

  pure $ next ()

runAppL :: R.AppRuntime -> L.AppL a -> IO a
runAppL appRt = foldFree (interpretAppF appRt)
