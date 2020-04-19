module Hydra.Core.Domain.Tea where

import           Hydra.Prelude
import           Hydra.Core.Domain.State (StateVar, SignalVar)


data TeaToken = TeaToken
  { _finished :: SignalVar
  }


data TeaAction
  = TeaFinish (Maybe String)
  | TeaLoop
  | TeaOutputMsg String

teaFinishedToken :: TeaToken -> SignalVar
teaFinishedToken token = _finished token
