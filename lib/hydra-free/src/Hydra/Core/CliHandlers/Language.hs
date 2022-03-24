
module Hydra.Core.CliHandlers.Language
  ( CliHandlerF (..)
  , CliHandlerL
  , CmdStr
  , ParamsStr
  , ParamHandler
  , cmd
  , cmdMethod
  ) where

import           Hydra.Prelude

import qualified Data.Text as T

import qualified Hydra.Core.Lang.Language as L

type CmdStr = Text
type ParamsStr = Text

type ParamHandler = ParamsStr -> Either Text (L.LangL ())


data CliHandlerF next where
  Cmd :: CmdStr -> L.LangL () -> (() -> next) -> CliHandlerF next
  CmdMethod :: CmdStr -> ParamHandler -> (() -> next) -> CliHandlerF next

instance Functor CliHandlerF where
  fmap g (Cmd cmdStr method next) = Cmd cmdStr method (g . next)
  fmap g (CmdMethod cmdStr handler next) = CmdMethod cmdStr handler (g . next)

type CliHandlerL next = Free CliHandlerF next


makeParamHandler
  :: ( Read input
     )
  => CmdStr
  -> (input -> L.LangL ())
  -> ParamHandler
makeParamHandler cmd f = \strMsg -> case readMaybe $ T.unpack strMsg of
  Nothing    -> Left $ cmd <> ": invalid params: " <> strMsg
  Just input -> Right $ f input


cmd
  :: CmdStr
  -> L.LangL ()
  -> CliHandlerL ()
cmd cmdStr handler = liftF $ Cmd cmdStr handler id


cmdMethod
  :: ( Read input
     )
  => CmdStr
  -> (input -> L.LangL ())
  -> CliHandlerL ()
cmdMethod cmdStr f = liftF $ CmdMethod cmdStr (makeParamHandler cmdStr f) id
