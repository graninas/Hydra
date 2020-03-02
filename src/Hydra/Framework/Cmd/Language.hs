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

import           GHC.Generics
import qualified GHC.Generics               as GS

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
