Hydra
=====

Haskell framework for building apps.
- Web backends
- Desktop apps
- ...

Properties:
* Safe (more safe than bare IO)
* Good testability
* Separation of concerns, separation of business logic, interfaces and implementation
  * Inversion of Control, Opacity, Interface segregation...
  - interfaces: free monadic eDSLs: LangL, AppL, ...
  - implementation: free monadic interpreters
  - business logic: labyrinth, astro etc
  - interfaces (free monad eDSLs) are abstracted (don't have any impl details)
  - implementations (interpreters) know how to connect calls from business logic
  to real calls and native libraries.
* Layering

------------------------------------

Interfaces: hierarchical Free monadic languages

* AppL
  -
  -
  - evalLang (nests LangL scenarios)

* LangL is an interface for some common actions

  - getRandomInt
  - evalIO
  - throwException
  - evalState (nests StateL methods)

  - readVarIO   - read an STM-like variable
  - writeVarIO  - write an STM-like variable
  - newVarIO    - create an STM-like variable

* StateL
  - readVar
  - writeVar
  - newVar


interpret readVarIO ----> readTVar   (real STM calls)
interpret writeVarIO ----> writeTVar (real STM calls)
interpret newVarIO ---->  newTVar    (real STM calls)


STM (Software Transactional Memory)
treads
