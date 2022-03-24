module Hydra.Core.CliHandlers.Interpreter where

import           Hydra.Prelude

import qualified Data.Map as Map
import qualified Data.Text as T

import qualified Hydra.Core.CliHandlers.Language as L
import qualified Hydra.Core.Lang.Language as L


-- Converting the Free monadic list of methods into a methods list

type CmdLine = Text

data CmdHandlerResult
  = CmdHandler (L.LangL ())
  | CmdHandlerNotFound L.CmdStr
  | CmdHandlerParamsError CmdLine

type Handlers = [(L.CmdStr, (L.ParamsStr -> Either Text (L.LangL ())))]

interpretCliHandlerL :: IORef Handlers -> L.CliHandlerF a -> IO a

interpretCliHandlerL handlersRef (L.Cmd cmdStr method next) = do
  let method' = \_ -> Right method
  modifyIORef' handlersRef (\l -> (cmdStr, method') : l)
  pure $ next ()

interpretCliHandlerL handlersRef (L.CmdMethod cmdStr method next) = do
  modifyIORef' handlersRef (\l -> (cmdStr, method) : l)
  pure $ next ()

runCliHandlerL :: IORef Handlers -> L.CliHandlerL () -> IO ()
runCliHandlerL handlersRef = foldFree (interpretCliHandlerL handlersRef)




getHandler :: Handlers -> CmdLine -> CmdHandlerResult
getHandler [] line = CmdHandlerNotFound line
getHandler ((cmd, handlerF):hs) line = case T.stripPrefix cmd line of
  Nothing         -> getHandler hs line
  Just restOfLine -> case handlerF restOfLine of
    Left errMsg   -> CmdHandlerParamsError errMsg
    Right handler -> CmdHandler handler
