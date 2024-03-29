{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE MultiWayIf          #-}

module Astro.Server
  ( runAstroServer
  ) where

import           Control.Monad
import           System.Process (readCreateProcess, shell)
import           Network.Wai.Handler.Warp (run)
import           Servant

import           Hydra.Prelude
import qualified "hydra-free" Hydra.Runtime as R
import qualified "hydra-free" Hydra.Interpreters as R
import qualified "hydra-free" Hydra.Language     as L

import           Astro.Config (loggerCfg, dbConfig)
import           Astro.ConsoleOptions (ServerOptions (..))
import qualified Astro.API as API
import           Astro.Domain.Meteor
import           Astro.Domain.Asteroid
import           Astro.Domain.Types
import           Astro.Domain.AstroObject
import           Astro.Catalogue
import           Astro.Types



data Env = Env !R.AppRuntime !AppState
type AppHandler = ReaderT Env (ExceptT ServerError IO)
type AppServer = ServerT API.AstroAPI AppHandler

astroServer :: Env -> Server API.AstroAPI
astroServer env = hoistServer API.astroAPI f astroServer'
  where
    f :: ReaderT Env (ExceptT ServerError IO) a -> Handler a
    f r = do
      eResult <- liftIO $ runExceptT $ runReaderT r env
      case eResult of
        Left err  -> throwError err
        Right res -> pure res

astroBackendApp :: Env -> Application
astroBackendApp = serve API.astroAPI . astroServer

runApp :: L.AppL a -> AppHandler a
runApp flow = do
  Env rt _ <- ask
  eRes <- lift $ lift $ try $ R.runAppL rt flow
  case eRes of
    Left (err :: SomeException) -> do
      liftIO $ putStrLn @String $ "Exception handled: " <> show err
      throwError err500
    Right res -> pure res


astroServer' :: AppServer
astroServer'
     = meteors
  :<|> meteor
  :<|> asteroid
  :<|> submitObjectTemplate
  :<|>
    ( getObject
    :<|> submitObjectOrbital
    :<|> submitObjectPhysical
    )

-- -- Sample of how to do straighforward validation:
-- submitObjectTemplate :: API.AstroObjectTemplate -> AppHandler AstroObjectId
-- submitObjectTemplate template@(API.AstroObjectTemplate {..}) = do
--
--   if | name == Just ""   -> throwError $ err400 {errBody = "Name should not be empty if specified."}
--      | objectClass == "" -> throwError $ err400 {errBody = "Object class should not be empty."}
--      | code == ""        -> throwError $ err400 {errBody = "Object code should not be empty."}
--      | otherwise         -> pure ()
--
--   runApp $ createObjectTemplate template

submitObjectTemplate :: API.AstroObjectTemplate -> AppHandler AstroObjectId
submitObjectTemplate template = runApp $ submitObjectTemplate' template

submitObjectTemplate' :: API.AstroObjectTemplate -> L.AppL AstroObjectId
submitObjectTemplate' template@(API.AstroObjectTemplate {..}) = do

  if | name == Just ""   -> failWith $ err400 {errBody = "Name should not be empty if specified."}
     | objectClass == "" -> failWith $ err400 {errBody = "Object class should not be empty."}
     | code == ""        -> failWith $ err400 {errBody = "Object code should not be empty."}
     | otherwise         -> pure ()

  createObjectTemplate template
  where
    failWith err = do
      L.logError $ show err
      L.scenario $ L.throwException err


getObject :: AstroObjectId -> AppHandler (Maybe AstroObject)
getObject = error "not implemented"

submitObjectPhysical :: AstroObjectId -> Physical -> AppHandler AstroObjectId
submitObjectPhysical = error "not implemented"

submitObjectOrbital :: AstroObjectId -> Orbital -> AppHandler AstroObjectId
submitObjectOrbital = error "not implemented"

meteors :: Maybe Int32 -> Maybe Int32 -> AppHandler Meteors
meteors mbMass mbSize = runApp
  $ withDB dbConfig
  $ getMeteors mbMass mbSize

meteor :: API.MeteorTemplate -> AppHandler MeteorId
meteor m = runApp
  $ withDB dbConfig
  $ createMeteor m

asteroid :: API.AsteroidTemplate -> AppHandler AsteroidId
asteroid _ = error "Not implemented yet."

prepareSQLiteDB :: IO ()
prepareSQLiteDB = do
  putStrLn @String "Copying astro_template.db to /tmp/astro.db..."
  eRes <- try $ void $ readCreateProcess (shell "cp ./app/astro/astro_template.db /tmp/astro.db") ""
  case eRes of
    Left (err :: SomeException) -> do
      putStrLn @String $ "Exception got: " <> show err
      error $ "Exception got: " <> show err
    Right _ -> pure ()

runAstroServer :: ServerOptions -> IO ()
runAstroServer opts = do
  prepareSQLiteDB
  putStrLn @String $ "Starting Astro App Server on port " ++ show port ++ "..."
  R.withAppRuntime (Just loggerCfg) $ \rt -> do
    appSt <- R.runAppL rt $ initState AppConfig
    run port $ astroBackendApp $ Env rt appSt
      where
        port = soListenPort opts
