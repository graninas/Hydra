Astro server and client
=======================

This is a service for reporting astronomical events.

It's supposed that there is a dedicated RESTful server that receives
requests from many HTTP / TCP clients all around the world.

Server
------

Servant-based HTTP server.

[Server.hs](./app/astro/src/Astro/Server.hs)
[Servant API](./app/astro/src/Astro/API.hs)

Running:

`$ astro server`

Client
------

Client app demonstrates the approaches to Dependency Injection on the business logic layer.

DI approaches:

- [FinalTagless](./app/astro/src/Astro/Client/FinalTagless.hs) | label: **FT**
- [FinalTagless (opt 2)](./app/astro/src/Astro/Client/FinalTagless2.hs) | label: **FT2**
- [Free Monad](./app/astro/src/Astro/Client/FreeMonad.hs) | label: **FM**
- [GADT](./app/astro/src/Astro/Client/GADT.hs) | label: **GADT**
- [ReaderT](./app/astro/src/Astro/Client/ReaderT.hs) | label: **RT**
- [ServiceHandle](./app/astro/src/Astro/Client/ServiceHandle.hs) | label: **SH**

You can choose the channel to send data:

- **HTTP**
- **TCP** (the logic is not implemented, just a demo of injecting this dependency)

Running:

`$ astro client`

Command line options:

  --approach ARG           approach one of:
                           [SH,RT,FM,FT,FT2,CEFM,GADT] (default: SH)
  --channel ARG            channel: http or tcp (default: HttpChannel)
  -h,--help                Show this help text,ExitSuccess,80))
