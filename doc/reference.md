# Reference guide

### Registry

The following combinators are available to create registries

###### Appending

 combinator             | meaning
 ---------------------- | -------
   `end`                | the empty registry
   `+:`                 | append an element to the registry
   `<+>`                | append 2 registries together

###### Registry elements

 combinator             | meaning
 ---------------------- | -------
   `val @a`             | a value of type `a` which can be added to the registry
   `fun @f`             | a function of type `f` which can be added to the registry
   `valM @m @a`         | a value of type `a` which is added as `m a` to the registry
   `pureM @m @f`        | a function of type `i1 -> i2 -> ... -> o` which is lifted into `m i1 -> m i2 -> ... -> m o` before being added to the registry
   `funM @m @f`         | a function of type `i1 -> i2 -> ... -> m o` which is lifted into `m i1 -> m i2 -> ... -> m o` before being added to the registry

###### Tweaking the registry

 combinator             | meaning
 ---------------------- | -------
   `singleton @m @a`    | if a value of type `m a` is created, store the value `a` so that the same `a` is returned whenever `m a` is executed
   `specialize @a @b b` | when trying to build a value of type `a` make sure that `b` is always used when a value of type `b` is required
   `tweak @a f`         | modify a value of type `a` with a function `f :: a -> a` right after it has been created and before storing it

###### Type aliases

 alias             | meaning
 ----------------- | -------
   `out :- a `     | `Contains a out` means that `a` is in the list of types `out`
