{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}

-- TODO: rework.
module Hydra.Framework.Cmd.Language
  ( CmdHandlerF (..)
  , CmdHandlerL
  , userCmd
  ) where

import           Hydra.Prelude
import qualified Data.Text          as T
import           Data.Typeable
import           Data.Data
import           Data.Default

import qualified Hydra.Core.Language as L

data CmdHandlerF next where
  UserCmd :: (Text -> Maybe a) -> (a -> L.LangL ()) -> (() -> next) -> CmdHandlerF next

instance Functor CmdHandlerF where
    fmap g (UserCmd parser cont next) = UserCmd parser cont (g . next)

type CmdHandlerL a = Free CmdHandlerF a

userCmd
  :: forall a
   . (Read a, Data a, Default a)
  => Text
  -> (a -> L.LangL ())
  -> CmdHandlerL ()
userCmd cmd handler = liftF $ UserCmd fParse handler id
  where
    cName = T.toLower $ toText $ show @String $ toConstr $ def @a
    fParse :: Text -> Maybe a
    fParse line = do
            t <- T.stripPrefix cmd $ T.stripStart line
            readMaybe $ toString $ T.concat [toText cName, " ", t]
