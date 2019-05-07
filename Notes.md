

1. Effects list vs type class hierarchy
```
meteorShower
  :: (L.ControlFlowL m, L.RandomL m, L.LoggerL m)
  => AppState -> Region -> m ()
```

```
class (L.ControlFlowL m, L.RandomL m, L.LoggerL m) => LangL m
```


2. Impossible embedded hierarchy
- Problems with StateL, ProcessL



3. Too much freedom

```
-- Impossible to say it should not do anything but state.
initState :: L.StateL m => m AppState
initState = ...

-- Someone can fix it:

initState :: (L.LoggerL m, L.StateL m) => m AppState
initState = ...

```

4. Hierarchy of runtimes breaks

5. Single runtime

FT obligates to have a single runtime per application (only a single monad stack, only a single runtime structure). It makes the app hard coupled

6. Endless type checking battles

```
Could not deduce (Hydra.Core.Evaluable.Evaluable m'0)

Could not deduce (L.LangL m'0)
  arising from a use of ‘meteorCounter’
from the context: (L.ProcessL m, L.LangL m)
```
