{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}

-- TODO: rework.
module Hydra.Framework.Cmd.Language
  ( CmdHandlerF (..)
  , CmdHandlerL
  , stdHandler
  , userCmd
  ) where

import           Hydra.Prelude
import qualified Data.Text          as T
import           Data.Typeable
import           Data.Data
import           Data.Char
import           Data.List
import           Data.Default

import qualified Hydra.Core.Language as L


toTag :: Typeable a => a -> String
toTag = takeWhile (/= ' ') . show . typeOf


data CmdHandlerF next where
  UserCmd :: (String -> Either String a) -> (a -> L.LangL ()) -> (() -> next) -> CmdHandlerF next
  CmdHandler :: String -> (String -> L.LangL String) -> (() -> next)  -> CmdHandlerF next

instance Functor CmdHandlerF where
  fmap g (UserCmd parser cont next) = UserCmd parser cont (g . next)
  fmap g (CmdHandler text f next) = CmdHandler text f (g . next)

type CmdHandlerL a = Free CmdHandlerF a


















userCmd
  :: forall a
   . (Read a, Data a, Default a)
  => String
  -> (a -> L.LangL ())
  -> CmdHandlerL ()
userCmd cmd handler = liftF $ UserCmd fParse handler id
  where
    cName = show @String $ toConstr $ def @a
    fParse :: String -> Either String a
    fParse line = case stripPrefix cmd $ dropWhile (== ' ') line of
      Nothing       -> Left $ "Cmd not parsed: " +|| (cmd, cName, line) ||+ ""
      Just stripped -> case readMaybe $ concat [cName, " ", stripped] of
        Nothing -> Left $ "Args not parsed: " +| concat [cName, " ", stripped] |+ ""
        Just r  -> Right r



-- userCmd
--   :: forall a
--    . (Read a, Data a, Default a)
--   => Text
--   -> (a -> L.LangL ())
--   -> CmdHandlerL ()
-- userCmd cmd handler = liftF $ UserCmd fParse handler id
--   where
--     cName = show @Text $ toConstr $ def @a
--     fParse :: Text -> Either Text a
--     fParse line = case T.stripPrefix cmd $ T.stripStart line of
--       Nothing -> Left $ "Cmd not parsed: " +|| (cmd, cName, line) ||+ ""
--       Just stripped  -> case readMaybe $ toString $ T.concat [cName, " ", stripped] of
--         Nothing -> Left $ "Args not parsed: " +| T.concat [cName, " ", stripped] |+ ""
--         Just r  -> Right r


stdHandler
  :: (Typeable a, Read a)
  => (a -> L.LangL String)
  -> CmdHandlerL ()
stdHandler f = liftF $ CmdHandler (toTag f) makeStdHandler id
  where
    makeStdHandler :: String -> L.LangL String
    makeStdHandler raw = case readMaybe raw of
      Just req -> f req
      Nothing  -> pure "Error of request parsing"
