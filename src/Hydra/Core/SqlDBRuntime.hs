module Hydra.Core.SqlDBRuntime where

import           Hydra.Prelude

import qualified Data.Map                        as Map

import qualified Hydra.Core.Domain               as D
import qualified Hydra.Core.Language             as L

import qualified Database.SQLite.Simple          as SQLite
import           Database.Beam.Sqlite (Sqlite)

type SQLiteDBConn   = MVar SQLite.Connection
type SQLiteDBConns  = TMVar (Map D.DBName SQLiteDBConn)

initSQLiteDB'
  :: SQLiteDBConns
  -> D.SqlDBConfig Sqlite
  -> IO (D.DBResult (D.SqlDBHandle Sqlite))
initSQLiteDB' connsVar cfg@(D.SQLiteConfig dbName) = do
  eConn <- try $ SQLite.open dbName
  case eConn of
    Left (err :: SomeException) -> pure $ Left $ D.DBError D.SystemError $ show err
    Right conn -> do
      dbM <- newMVar conn
      atomically $ do
        conns <- takeTMVar connsVar
        putTMVar connsVar $ Map.insert dbName dbM conns
      pure $ Right $ D.mkSQLiteHandle dbName

deInitSQLiteConn :: SQLiteDBConn -> IO ()
deInitSQLiteConn connVar = do
  conn <- takeMVar connVar
  SQLite.close conn
  putMVar connVar conn

closeSQLiteConns :: SQLiteDBConns -> IO ()
closeSQLiteConns handleMapVar = do
  handleMap <- atomically $ takeTMVar handleMapVar
  mapM_ deInitSQLiteConn $ Map.elems handleMap
  atomically $ putTMVar handleMapVar Map.empty
