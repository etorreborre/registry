# Boxes

##### *It's boxes all the way down* <img src="doc/images/unboxed-bottomup.jpg" border="0"/>

#### Presentation

This library provides a data structure, a `Registry`, to control the creation of functions from other functions. There are numerous applications of this technique:

 - to create applications out of software components
 - to fine tune JSON encoders/decoders
 - to create a DSL "language" for data generators

The following sections introduce in more details the problem that this library is addressing, the concepts behind the solution and various use-cases which can arise on real projects:

 1. [what is the problem?](docs/motivation.md)
 2. the concept of a [Registry](docs/registry.md) and the resolution algorithm
 3. using a `Registry` to fine-tune [JSON encoders](doc/generators.md) with `HedgeHog`
 4. using a `Registry` to create [applications](doc/applications.md) and define components
 6. [implementation notes](doc/implementation.md)

### More information

 1. how to do mocking?
 1. how to deal with libraries?
 1. how to specialize some values in some contexts?
 1. how to make a singleton for a database?
 1. how to allocate resources which must be finalized?
 1. how to initialize components in an application?
 1. reference guide with all the operators
