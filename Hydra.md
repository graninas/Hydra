

The Hydra project layers:


0. Base monads for the framework:
  - Free monad engine
  - Church-encoded Free monad engine (not complete)
  - IO

1. Hydra framework
  Exposes several monads: AppL, LangL, ....

2. Applications


Labyrinth, based on the Hydra (its Free monad engine)
- Business Logic - scenarios in the AppL, LangL, RandomL monads
- Interfaces, eDSL: AppL, LangL... monads (Free monads)
- Implementation & Runtime (IO monad)


Labyrinth, based on the Hydra (its Church Encoded Free monad engine)
- Business Logic - scenarios in the AppL, LangL, RandomL monads
- Interfaces, eDSL: AppL, LangL... monads (Church Encoded Free monads)
- Implementation & Runtime (IO monad)

--------------------------------------------------------------------------------

Approaches to Architecture

1. IO monad
2. Final Tagless / mtl (based on type classes)
3. ReaderT pattern (monad transformers)
4. Service Handle pattern (Handle pattern, Service pattern)
5. Extensible effect systems (type level lists of effects)
6. Free monads, Church-Encoded Free monads
