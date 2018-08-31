# Registry

##### *It's functions all the way down* <img src="doc/images/unboxed-bottomup.jpg" border="0"/>

#### Presentation

This library provides a data structure, a `Registry`, to control the creation of functions from other functions. You can use this technique to:

 - create applications out of software components ("dependency injection")
 - fine tune JSON encoders/decoders
 - create composable data generators for nested datatypes

The following sections introduce in more details the problem that this library is addressing, the concepts behind the solution and various use-cases which can arise on real projects:

 1. [what is the problem?](doc/motivation.md)
 1. the concept of a [Registry](doc/registry.md) and the resolution algorithm

#### Tutorials

 1. use a `Registry` to compose [Hedgehog generators](doc/generators.md)
 1. use a `Registry` to create [applications](doc/applications.md) and define components

#### How-tos

 1. how to install this library?
 1. how to do mocking?
 1. how to put code in other libraries?
 1. how to specialize some values in some contexts?
 1. how to make a singleton for a database?
 1. how to allocate resources which must be finalized?
 1. how to initialize components in an application?

#### Reference guides

 1. all operators and main functions
 1. [implementation notes](doc/implementation.md)
