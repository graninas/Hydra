{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}

-- TODO: rework.
module Hydra.Framework.Cmd.Language
  ( CmdHandlerF (..)
  , CmdHandlerL
  , CmdOutput (..)
  , stdHandler
  , simpleCmd
  , simpleCmd_
  , userCmd
  , userCmd_
  ) where

import           Hydra.Prelude
import qualified Data.Text          as T
import           Data.Typeable
import           Data.Data
import           Data.Char
import           Data.List
import           Data.Default

import           GHC.Generics
import qualified GHC.Generics as GS

import qualified Hydra.Core.Language as L

data CmdOutput
  = ArgsNotParsed String
  | SkipCmd String
  deriving (Eq, Show)

data CmdHandlerF next where
  UserCmd    :: (String -> Either CmdOutput a) -> (a -> L.LangL (Maybe String)) -> (() -> next) -> CmdHandlerF next
  CmdHandler :: String -> (String -> L.LangL (Maybe String)) -> (() -> next) -> CmdHandlerF next

instance Functor CmdHandlerF where
  fmap g (UserCmd parser cont next) = UserCmd parser cont (g . next)
  fmap g (CmdHandler text f next) = CmdHandler text f (g . next)

type CmdHandlerL a = Free CmdHandlerF a



-- data FieldType
--   = FStr
--   | FInt
--   deriving (Show)
--
-- data FieldDef = FieldDef String FieldType
--   deriving (Show)
--
-- class ToFieldDef1 f where
--   toFieldDef1 :: Proxy f -> [FieldDef]
--
-- instance (ToFieldDef1 f) => ToFieldDef1 (GS.M1 D t f) where
--   toFieldDef1 _ = toFieldDef1 (Proxy :: Proxy f)
--
-- instance (ToFieldDef1 f) => ToFieldDef1 (GS.C1 c f) where
--   toFieldDef1 _ = toFieldDef1 (Proxy :: Proxy f)
--
-- instance (ToFieldDef1 k1, Selector s) => ToFieldDef1 (GS.M1 GS.S s k1) where
--   toFieldDef1 _ = fs'
--     where
--       FieldDef fn ft : fs = toFieldDef1 (Proxy :: Proxy k1)
--       fn' = GS.selName (undefined :: M1 S s k1 k)
--       fs' = FieldDef fn' ft : fs
--
-- instance ToFieldDef1 (GS.K1 GS.R String) where
--   toFieldDef1 _ = [FieldDef "" FStr]
--
-- instance ToFieldDef1 (GS.K1 GS.R Int) where
--   toFieldDef1 _ = [FieldDef "" FInt]
--
-- instance
--   (ToFieldDef1 f, ToFieldDef1 g)
--   => ToFieldDef1 ((:*:) f g) where
--   toFieldDef1 _ = toFieldDef1 (Proxy :: Proxy f) ++ toFieldDef1 (Proxy :: Proxy g)
--
-- defaultToFieldDef
--   :: forall a
--   . (Generic a, ToFieldDef1 (Rep a))
--   => Proxy a
--   -> [FieldDef]
-- defaultToFieldDef _ = toFieldDef1 (Proxy :: Proxy (Rep a))
--


simpleCmd
  :: String
  -> L.LangL (Maybe String)
  -> CmdHandlerL ()
simpleCmd cmd handler = liftF $ UserCmd fParse (const handler) id
  where
    fParse :: String -> Either CmdOutput ()
    fParse line = case stripPrefix cmd $ dropWhile (== ' ') line of
      Nothing       -> Left $ SkipCmd ("Cmd not parsed: " +|| (cmd, line) ||+ "")
      Just stripped -> Right ()

simpleCmd_
  :: String
  -> L.LangL (Maybe String)
  -> CmdHandlerL ()
simpleCmd_ cmd handler = simpleCmd cmd (handler $> Nothing)

-- | Experimental. Works with only ADT types without field selectors.

userCmd
  :: forall a
   . (Read a, Data a, Default a)
  => String
  -> (a -> L.LangL (Maybe String))
  -> CmdHandlerL ()
userCmd cmd handler = liftF $ UserCmd fParse handler id
  where
    cName = show @String $ toConstr $ def @a
    fParse :: String -> Either CmdOutput a
    fParse line = case stripPrefix cmd $ dropWhile (== ' ') line of
      Nothing       -> Left $ SkipCmd ("Cmd not parsed: " +|| (cmd, cName, line) ||+ "")
      Just stripped -> case readMaybe $ concat [cName, " ", stripped] of
        Nothing -> Left $ ArgsNotParsed ("Args not parsed: " +| concat [cName, " ", stripped] |+ "")
        Just r  -> Right r

userCmd_
  :: forall a
   . (Read a, Data a, Default a)
  => String
  -> (a -> L.LangL ())
  -> CmdHandlerL ()
userCmd_ cmd handler = userCmd cmd (\r -> handler r $> Nothing)



toTag :: Typeable a => a -> String
toTag = takeWhile (/= ' ') . show . typeOf

stdHandler
  :: (Typeable a, Read a)
  => (a -> L.LangL (Maybe String))
  -> CmdHandlerL ()
stdHandler f = liftF $ CmdHandler (toTag f) makeStdHandler id
  where
    makeStdHandler :: String -> L.LangL (Maybe String)
    makeStdHandler raw = case readMaybe raw of
      Just req -> f req
      Nothing  -> pure Nothing
