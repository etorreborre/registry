# Registry [![Hackage](https://img.shields.io/hackage/v/registry.svg)](https://hackage.haskell.org/package/registry) [![Build Status](https://github.com/etorreborre/registry/workflows/CI/badge.svg)](https://github.com/etorreborre/registry/actions)


[![Join the chat at https://gitter.im/etorreborre/registry](https://badges.gitter.im/etorreborre/registry.svg)](https://gitter.im/etorreborre/registry?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

##### *It's functions all the way down* <img src="doc/images/unboxed-bottomup.jpg" border="0"/>

#### Presentation

This library provides a data structure, a `Registry`, to control the creation of functions from other functions. You can use this technique to:

 - create applications out of software components ("dependency injection")
 - fine tune JSON encoders/decoders
 - create composable data generators for nested datatypes

You can watch a video presenting the main ideas behind the library [here](https://skillsmatter.com/skillscasts/12299-wire-once-rewire-twice).

The following sections introduce in more details the problem that this library is addressing, the concepts behind the solution and various use-cases which can arise on real projects:

 1. [what is the problem?](doc/motivation.md)
 1. the concept of a [Registry](doc/registry.md) and the resolution algorithm
 1. how does this [compare to monad and effects?](https://github.com/etorreborre/effects)

#### Tutorials

 1. tutorial 2: use a `Registry` to create [applications](doc/tutorial.md) and define components
 1. use a `Registry` to compose [Hedgehog generators](doc/generators.md)

#### How-tos

 1. how to [install this library](doc/install.md)?
 1. how to do [mocking](doc/applications.md#integration)?
 1. how to [specialize some values in some contexts](doc/applications.md#context-dependent-configurations)?
 1. how to [control effects](doc/applications.md#memoization) occurring when creating a component (like a connection pool)?
 1. how to [allocate resources](doc/applications.md#resources) which must be finalized?
 1. how to [initialize components](doc/applications.md#start-up) in an application?
 1. how to [extract a dot graph from the registry](doc/dot.md) in an application?
 1. how to [interact with a library using monad transformers](https://github.com/etorreborre/registry/blob/master/test/Test/Data/Registry/MonadRandomSpec.hs)?
 1. how to [remove boilerplate](doc/boilerplate.md) due to parameter passing?
 1. how to [create a typeclass from a record of functions](doc/typeclass.md)?

#### Reference guides

 1. [main operators and functions](doc/reference.md)
 1. [implementation notes](doc/implementation.md)
