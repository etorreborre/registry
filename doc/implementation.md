# Implementation notes

The implementation relies on several pieces which I document here:

 - the use of `Dynamic` to store values and functions in the registry.
   Thanks to `Dynamic.applyDyn` it is possible to apply an untyped function
   to a list of untyped arguments (see `Data.Registry.Internal.Registry`)

 - the lifting of pure functions to functions having monadic arguments
   regardless of the arity of the function (see `Data.Registry.Lift`)

 - the use of type-level lists to track the inputs and outputs of a registry,
     with the use of some type families and type classes to be able to
     statically say if a value can be built or not

 - the use of an `MVar` to cache singleton values when they are built with `IO` actions
   (see `Data.Registry.Internal.Cache`)
