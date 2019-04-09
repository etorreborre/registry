# Reference guide

### Registry

The following combinators are available to create registries

###### Appending

 combinator             | meaning
 ---------------------- | -------
   `end`                | the empty registry
   `+:`                 | append an element to the registry
   `<+>`                | append 2 registries together

###### Creating registry elements

 combinator             | meaning
 ---------------------- | -------
   `val @a`             | a value of type `a` which can be added to the registry
   `fun @f`             | a function of type `f` which can be added to the registry
   `valTo @m @a`        | a value of type `a` which is added as `m a` to the registry
   `funTo @m @f`        | a function of type `i1 -> i2 -> ... -> o` which is lifted into `m i1 -> m i2 -> ... -> m o` before being added to the registry
   `funAs @m @f`        | a function of type `i1 -> i2 -> ... -> m o` which is lifted into `m i1 -> m i2 -> ... -> m o` before being added to the registry (mnemonic the `A` stands for "arguments")


###### Lifting functions

It is also possible to only use `val` and `fun` and lift functions yourself with the following combinators:

 combinator             | meaning
 ---------------------- | -------
   `allTo @m`           | lift a function of type `i1 -> i2 -> ... -> o` to `m i1 -> m i2 -> ... -> m o`
   `argsTo @m`          | lift a function of type `i1 -> i2 -> ... -> m o` to `m i1 -> m i2 -> ... -> m o`
   `outTo @m nat`       | lift a function of type `i1 -> i2 -> ... -> n o` to `m i1 -> m i2 -> ... -> m o` using `nat :: forall x . n x -> m x`

###### Making elements

 combinator             | meaning
 ---------------------- | -------
   `make @a`            | statically check that an element can be built and build it
   `makeFast @a`        | statically check that an element is one of the registry outputs and build it
   `makeEither @a`      | make a value and return `Left <an error>` if the value cannot be built
   `makeUnsafe @a`      | make a value and throw an exception if the value cannot be built

###### Speed-up compilation times

 combinator                    | meaning
 ----------------------        | -------
  `normalize`                  | make the list of types in the registry unique (no duplicated types) to speed-up compile times with `make`
  `$(checkRegistry 'registry)` | check that any output type in the registry can be built (this uses TemplateHaskell). Once a registry is checked `makeFast` can safely be used

###### Tweaking the registry

 combinator                    | meaning
 ----------------------        | -------
  `memoize @m @a`              | if a value of type `m a` is created, store the value `a` so that the same `a` is returned whenever `m a` is executed
  `memoizeAll @m`              | run `memoize` for all the effectful output types of a registry
  `specialize @a @b b`         | when trying to build a value of type `a` make sure that `b` is always used when a value of type `b` is required
  `specializeVal @a @b b`      | similar to `specialize` but uses `Show b` to display a better description when printing the registry out
  `specializeValTo @m @a @b b` | similar to `specializeVal` but "lifts" `b` into an `Applicative` context
  `specializePath @[as] @b b`  | specialize a value but only for a given "path of types" when those types are part of the current search context
  `tweak @a f`                 | modify a value of type `a` with a function `f :: a -> a` right after it has been created and before storing it

###### Type aliases

 alias             | meaning
 ----------------- | -------
   `out :- a `     | `Contains a out` means that `a` is in the list of types `out`
