module Hydra.Core.SqlDBRuntime where

import           Hydra.Prelude

import qualified Data.Map                        as Map

import qualified Hydra.Core.Domain               as D
import qualified Hydra.Core.Language             as L

import qualified Database.SQLite.Simple          as SQLite


type SQLiteDBConn   = MVar SQLite.Connection
type SQLiteDBConns  = TMVar (Map D.DBName SQLiteDBConn)

initSQLiteDB'
  :: SQLiteDBConns
  -> D.SqlDBConfig
  -> IO (D.DBResult D.SqlDBHandle)
initSQLiteDB' connsVar cfg@(D.SQLiteConfig dbName) = do
  eConn <- try $ SQLite.open dbName
  case eConn of
    Left (err :: SomeException) -> pure $ Left $ D.DBError D.SystemError $ show err
    Right conn -> do
      dbM <- newMVar conn
      atomically $ do
        conns <- takeTMVar connsVar
        putTMVar connsVar $ Map.insert dbName dbM conns
      pure $ Right $ D.SQLiteHandle D.SQLite dbName

deInitSQLiteDB :: SQLiteDBConn -> IO ()
deInitSQLiteDB connVar = do
  conn <- takeMVar connVar
  void $ try $ SQLite.close conn
  putMVar connVar conn

closeSQLiteDBs :: SQLiteDBConns -> IO ()
closeSQLiteDBs handleMapVar = do
  handleMap <- atomically $ takeTMVar handleMapVar
  mapM_ deInitSQLiteDB $ Map.elems handleMap
  atomically $ putTMVar handleMapVar Map.empty
