# Box

This project provides a way to structure Haskell applications and libraries
as a collection of software components.

The objectives are:

 - being able to declare components having a public interface which is just a list
   of functions (for information hiding)

 - have simple function definitions, with mostly `IO` instead of mtl constraints or Monad transformers
   (for an easy interoperability of different components)

 - have components declare their dependencies and configuration at their declaration
   site (to be able to put them in a library)

 - have a way to easily wire a full application from a list of all wanted components
   and configurations. If a new dependency is added to a component, we shouldn't
   have to change the wiring code (for ease of maintenance).

   Ideally there should be a way to declare how components sharing the same
   type of dependency must be wired:
   ```
     // -> is "depends on"
     B -> D; and C -> D
   ```
   Now if I want `A -> (B, C)` do I get one `D` or 2? In general we want singletons
   (meaning `D` must just be present once) but we might want to duplicate components
   sometimes

 - have a way to replace specific components in an existing application
   (for testing)

 - have a way to handle resources for stateful components

The name used in the library to talk about such components is a `Box`:

 - it is a simple object, with a simple shape
 - it does what is written on its label
 - you cannot open it
 - you can replace it with another box with the same label
 - this is a short name and does not conflict with the haskell `module` keyword

### Information hiding

Each box must be declared in a separate Haskell source file
and declare a `Box` data structure containing its interface.

For example in `Calculator.hs`:
```haskell
data Box =
  Box
  { add ::      Int -> Int -> RIO Int
  , multiply :: Int -> Int -> RIO Int
  }
```

### Interoperability

This library promotes the use of very simple functions where return types are kept to the minimum:

 - static configuration parameters should be passed when building the boxes (so no use for `MonadReader` or `ReaderT`)

 - logging can be done by depending on an explicit logging box

 - any other kind of "capability" (database access, file system access,...) is represented by a specific box wired
    into other boxes needing them

 - effects, or stateful operations are represented by the `IO` type

There is a catch though. Features like logging or passing a "Flow Id" across components invokations fundamentally need
some form of reader pattern. One option would be to create a type `e -> IO a` where `e` represents some sort of environment.
The downsides are the following:

 - this opens the door to using `e` to define some "capabilities"
 - this make interoperability harder when having to deal with different environment types

For those reasons this library defines a datatype for interoperability:
```
newtype RIO a = RIO { runRIO :: Env -> IO a }
```

where `Env` is a dynamic object encoding untyped (in the form of a JSON value) additional knowledge from the caller:

 - for example to pass a `FlowId`
 - or to describe some nested processes:
   `processing event { "eventId" = "123" } > getting master data > authenticating { "role" = "admin" }`

The obvious drawback is that the typechecker will not help you if you forgot to set-up a "Flow Id" for
example before calling one component.

### Dependencies

Inside each Box file there should be a declaration for the
box configuration
```
data Config =
  Config
  { invertSigns :: Boolean }
```
Dependencies are declared with a function `new` taking:

 - a `Config` value
 - other boxes

And returning a `Box` containing all the box functions.
Other boxes must be imported `qualified` in order to use their own
`Box` datatypes:
```
new :: Config -> Logging.Box -> Adder.Boxx -> Multiplier.Box -> Box
new (Config invert) logging adder multiplier =
  Box (addWith invert logging adder)
      (multiplyWith logging multiplier)

addWith :: Bool -> Adder.Box -> Int -> Int -> RIO Int
addWith True adder a b = pure (adder & add a b)
addWith False adder a b = pure (- (adder & add a b))

...
```

### Wiring

#### Preparing boxes

Some typeclasses are provided to help with wiring applications and libraries:

  - `Register s Box` means that there is a possibility
    to get the Box from a registry `s` (and possibly get nothing)
    and also to add it to the registry `s`

  - `Make s m` means that there is a way to build the box `m`
     given an initial configuration `s`

For a given box a `Make` instance can be created by using various helper methods.
for example, using the previously defined `new` method:
```haskell
import Data.Box.Make
...

instance ( Register s Box
         , Make s Config
         , Make s Logging.Box
         , Make s Adder.Box
         , Make s Multiplier.Box
         ) => Make s Box where
  make = create4 new
```

#### Box creation

A special datatype `BoxRIO` is used to create instances. It helps with;

 - resources allocation and automatic cleanup
 - box warmup (service check, cache pre-loading,...)

#### Resources allocation

Some boxes allocate resources when they are built. Those boxes can use the
`allocate` function to create those resources and register a finalizer which will
be called when the box is not used anymore
```haskell
new :: Config -> BoxRIO Box
new (Config url) = do
  connection <- allocate (newConnection url) closeConnection
  pure $ Box ({-# use the connection here #-})
```

#### Warmup

Additionally when creating a box it is possible to use `warmupWith` to declare
a "warmup" action which will check that the box is functioning properly.
Please read `Data.Box.Warmup` for some examples on how to create `Warmup`
values.

## Creating an application

In order to create a full application we need:

 - a `Config` datatype holding all the relevant configuration parameters

 - a top-level component `App`

 - `Make` instances for all the components

 - a `Registry` datatype with instance definitions for all the components

Here is an example of `Config` datatype:
```haskell
data Config
  Config
  { _loggingConfig    :: Logging.Config
  , _adderConfig      :: Adder.Config
  , _multiplierConfig :: Multiplier.Config
  , _calculatorConfig :: Calculator.Config
  }
```

The `App` can just be the `Calculator` so the only thing now needed is a registry.

### Create a registry with the `Registry` datatype

The `Data.Make.Registry` datatype can hold configuration values and "slots" for boxes
to be created.

An "empty" `Registry` can be obtained by using the `registry` function for a given set
of types:
```haskell
import Data.Make.Registry

type AppRegistry = Registry '[
    Logging.Box
  , Adder.Box
  , Multiplier.Box
  , Calculator.Box
  ] '[]

defaultRegistry :: AppRegistry
defaultRegistry = registry

prodConfig =
     loggingConfig
  +: adderConfig
  +: multiplierConfig
  +: calculatorConfig
  +: end

-- | This registry can be used to create any box in production
prodRegistry = defaultRegistry `add` prodConfig
```

## Start the application

The application can be started with the `withBox` function:

```haskell
app :: (Calculator -> RIO Int) -> RIO Int
app f = withBox prodRegistry $ \result calculator ->
  if isSuccess result then f calculator
  else print "warmup did not succeed :-("
```

The `withBox` function (coming from `Data.Box.Make`) does several things:

 - it creates an instance invoking the `make` function for the `Calculator` with a given
   registry. This recursively calls the `Make` instances of all the dependencies

 - it starts the warmup function of each box

 - it passes the warmup result + the built box so that you can decide how to
 exit in case of failure or how to continue

### Testing

For testing we can easily replace a box built from nothing by one
directly set on the registry:
```haskell
zeroAdder :: Adder.Box
zeroAdder =
  Adder.Box
  { add _ _ = 0 }

-- | This registry can now be used to get an Adder.Box
testingCalculatorRegistry =
  zeroAdder `override` prodRegistry
```

For testing we can also "short-circuit" the warm-up phase and delay
the resource cleanup if necessary. Please have a look at the various
functions in `Data.Box.Make`.

### Create boxes in ghci

You can create a given box in ghci without having to build a `Box`
data type declaring all dependencies with the `makeBox` function:

 - this will *not* create singletons

 - you need to run `:set -XTypeApplications` in your ghci session (or add to ~/.ghci)

 - you need to have `Data.Default` instances for all the types having no further dependencies
   like configuration objects

For example the `Metrics` box can run with a default configuration on `localhost:8080`
and does not depend on much else so you can use `makeBox` to invoke it.

```haskell
λ> import Data.Box.RIO
λ> import Data.Box.Make
λ> import Data.Box.Warmup
λ> import Data.Metrics as M
λ>
λ> (m,w,s) <- runLiftIO $ makeBox @M.Box
λ> runLiftIO $ runStop s
```

Remember that `w` is the "warmup" value for the box (`runWarmup w` just returns `Empty` here)
and `s` is the `stop` value for the box
