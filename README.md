The Hydra Project
=================

This project demonstrates the principles of Software Design and Architecture in pure Functional Programming. It provides a framework implemented with several different approaches for easy comparison:

- Final Tagless
- Free Monad
- Church Encoded Free Monad

I've made a talk "Final Tagless vs Free Monad" and described the discoveries from this project.
- [Slides (Rus)](https://docs.google.com/presentation/d/1VhS8ySgk2w5RoN_l_Ar_axcE4Dzf97zLw1uuzUJQbCo/edit?usp=sharing)
- [Talk "Final Tagles vs Free Monad" (Rus)](https://www.youtube.com/watch?v=u1GGqDQyGfc)

Hydra Framework
---------------

An all-in-one framework for writing Haskell apps which use the following features out of the box:

- Multithreading
- Safe STM-powered concurrency
- KV DB functionality (Redis & RocksDB backends supported)
- SQL DB functionality (beam incorporated, SQLite supported, PG & MySQL possible)
- Logging
- Random data generation
- Many others

With the framework, you can create complex applications that have a good maintainability, testability, simplicity, that have a good structure and are easy to read and change. The key principles in the framework:

- Layered architecture
- Separation of Subsystem Interfaces, Business Logic, Runtime and Implementation
- Convenient and simple to use eDSLs for Business Logic
- Layered structure of business application: State, Domain, Business Logic
