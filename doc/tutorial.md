# Registry tutorial

## Introduction

`registry` is a library offering an alternative to typeclasses for implicitly assembling functionalities in Haskell.
For example a typeclass for an `Encoder` will implicitly retrieve other `Encoder`s and use them to build a new one:
```haskell
instance (Encoder Int) => Encode Age where
  encode (Age n) = encode n
```

This can also be written "manually" as a function call:
```haskell
newtype Encoder a = Encode { encode :: a -> Text }

ageEncoder :: Encoder Int -> Encoder Age
ageEncoder intEncoder = Encoder (\Age n -> encode intEncoder n)
```

`registry` automates the call of such functions when it is necessary to build more complex structures like an application or data generators.
The benefits are:

 - the construction of instance is still "type-directed" as with typeclasses, you can make an instance by specifying its type only, like `Encoder Age`
 - the construction can be altered by injecting some hand-crafted values in order to replace normal components with mocks for example
 - it is also possible to inject values in a specific context only, for example a specific `Encoder Int` used by the `Encoder Age` but not by the `Encoder Year`

If you want to get a good mental picture of what a registry is, you can visualize:

 - an ordered list of values and functions
 - an algorithm applying those functions to values in order to obtain other values based on their type
 - some settings to "tweak" the algorithm (more on that later)

The algorithm goes like this:

 1. to get a value of type `a` check if there already is a value of that type in the registry and _take the first available one_
 2. otherwise check if there is a function returning the type `a` (and _take the first available one_)
 3. if there is one, create all the values required to call the function
 4. then call the function and get a value of type `a`

This is pretty straightforward and we can see that "taking the first available value" allows us to "override" the registry by adding any value
 (or function actually) "on top" since this is an ordered list.

Let's see on a few examples what this means for wiring an application and generating data.

## Application to components wiring

We are going to build, step by step, a very small application based on components being "records of functions". The purpose is to get
 comfortable with the `registry` API:

 - how to create a registry holding our component definitions and values
 - how to make a component with the registry
 - how to "override" a component to replace it with a mock
 - how to deal with components requiring some `IO` for their creation
 - how to "decorate" an existing component with some additional behavior
 - how to memoize effectful components
 - how to deal with typechecking for large registries

### Our first registry

For this tutorial it is advised to create one file per exercise, possibly reusing code from previous exercises by using imports.
You can then use the GHCi repl to make sure everything compiles and returns the values you expect.

We start with an application which does not do much, it asks the user if they want to know "the answer to life, the universe and everything"
and accepts the following answers:

 - `'Yes'` returns the answer by reading it from a file
 - `'No'` quits
 - `Maybe` draws a random boolean and either quits or returns the answer

We use the following components for this application:
```haskell
data App m = App {
  userInput    :: UserInput m
, secretReader :: SecretReader m
, rng          :: Rng m
, console      :: Console m
}

startApp :: App IO -> IO ()
startApp app@App{..} = do
  userAnswer <- askQuestion userInput
  quit <- case userAnswer of
            Maybe -> randomBool rng
            No    -> pure True
            Yes   -> pure False
  if quit then
    write console "bye"
  else do
    secret <- readSecret secretReader
    case secret of
      Nothing -> write console "Sorry I actually don't know"
      Just s -> do
        write console ("the answer is " <> s)
        startApp app

data Logger m = Logger {
  info  :: Text -> m ()
, error :: Text -> m ()
}

data UserAnswer = Yes | No | Maybe deriving (Eq, Show)

data UserInput m = UserInput {
  askQuestion :: m UserAnswer
}

data Rng m = Rng {
  randomBool :: m Bool
}

-- Get the secret answer and return Nothing if not found
data SecretReader m = SecretReader {
  readSecret :: m (Maybe Text)
}

data Console m = Console {
  write :: Text -> m ()
, read  :: m Text
}
```

Those components with their implementations are available in `test/Test/Tutorial/Application.hs`.

### Exercise 1

Implement the `newApp` function to build the `App` from the individual components by calling their respective constructors: `newConsole`, `newLogger`,...

### Exercise 2

Import `Data.Registry` and:

 1. create a registry containing all constructors and configuration values using `<:`, `fun`, `val`

```haskell
registry =
     ...
  <: fun newLogger
  <: fun newRng
  <: val (SecretReaderConfig ...)
```

Note the order in which you add elements to the registry matters!
You will get a compilation error if you are trying to add a function whose inputs
have not yet any way to be built by another function in the registry.

**Tip**

You can use the following pragmas to avoid typing the full signature of `registry`:
```haskell
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
```

Then:

 1. print the `registry` in the repl, what do you see?
 2. implement the `newApp` function by calling `make @App registry`
 3. `startApp newApp` in the repl!
 4. comment out one of the constructors in the registry and observe the error you get on recompilation
 5. now use `makeUnsafe` instead of `make` and recompile, the code should compile
 6. what happens when you try to execute `startApp newApp` though?

_Notes_:

 - `fun` is used to add a constructor function
 - `val` is used to add a configuration value, it must have a `Show` instance
 - the 2 lists of types `l1` and `l2` in `Registry l1 l2` are:
     1. the list of all the functions inputs
     2. the list of all the function outputs

    They are used to statically check if you can `make` a value of a given type out of the registry

### Exercise 3

Now use the previous `registry` and modify it to start the `App` with a `silentLogger`
```haskell
silentLogger = Logger (const (pure ())) (const (pure ()))
```

1. create a `silentRegistry` by adding the `silentLogger` to the `registry` using `fun silentLogger +: ...`
2. run a `newSilentApp'` using this registry and observe that answering `Maybe` to the question should not display the random boolean used to determine what to do.
3. modify to the `registry` to set an incorrect `SecretReaderConfig "missing"` value and run the `App`. When answering `Yes` you should see an error with this `newMisconfiguredApp`.
4. create a `newMisconfiguredSilentApp` which will be both misconfigured and will not output any logging
5. modify the `registry` to be both misconfigured and have a silent logger *only* for the `Rng IO` component with `specialize @(Rng IO) silentLogger`.
    You should still see error log messages but no info messages when selecting `Maybe`
6. go back to `Application.hs` and add a new `Logger` dependency to the `UserInput` component.
   Observe that the code still compiles, you have done a _local_ dependency modification without having to change any of the _global_ code wiring the application

### Exercise 4

Take any registry and output the dot graph for the `App` component:
```haskell
putStrLn $ unDot $ makeDot @App registry
```

Copy the output and paste it at http://www.webgraphviz.com

### Exercise 5

_Note_: this could be extended to the `ResourceT IO` monad for dealing with resource allocation

Now we are going to introduce another implementation for the `SecretReader` component.
We will now check right away if the secret file is missing or not, and emit an error right away
if it does not exist (don't bother trying to reuse code from `newSecretReader` for now)
```haskell
newCheckedSecretReader :: SecretReaderConfig -> Logger IO -> IO (SecretReader IO)
newCheckedSecretReader (SecretReaderConfig path) logger = do ...
```

1. create a registry, `registryIO`, with this new constructor. Since it is now in `IO` you must lift *everything*,
   all values and functions, to `IO` using `funTo @IO` and `valTo @IO` instead of `fun` and `val`
2. start the application with `startApp =<< newAppIO` using the new `registryIO` to make the `App`
3. experiment with this new setup by using an incorrect configuration

_Notes_:

 - in this exercise we introduce new combinators `funTo`, `valTo`, which makes the `registry` API a bit more complex
 - however the main algorithm has not changed at all, `registry` is still applying functions to values regardless of these values being in a monadic context or not
 - this also means that each invocation of a value `IO a` will execute the effect each time. See Exercise 7 on how to fix this

### Exercise 6 (advanced)

It is quite annoying that we could not reuse the previous `SecretReader` implementation to implement the checked one. Why does this not work?
```haskell
newCheckedSecretReader :: SecretReaderConfig -> Logger IO -> SecretReader IO -> IO (SecretReader IO)
newCheckedSecretReader config logger original = -- use the original version
```

1. use a "tagged" version of the `SecretReader` in `newCheckedSecretReader` (`unTag` will remove the tag from `Tag "tag" a` to return just `a`)
```haskell
newCheckedSecretReader :: SecretReaderConfig -> Logger IO -> Tag "unchecked" SecretReader IO -> IO (SecretReader IO)
```
2. add a "tagged" constructor to the `registry` using the `tag` function
```haskell
   funTo @IO (tag "unchecked" newSecretReader)
<: ...
```
This will allow the registry algorithm to distinguish between an "unchecked" `SecretReader` from a "checked" one since they have now different types.

3. experiment with this new setup by using an incorrect configuration

### Exercise 7 (advanced)

Another issue related to having effectful components is that effects can be executed several times.
This is clearly a problem for resources like connection pools where we don't want to create many times the same pool.

1. to convince yourself that this is the case add a `newInitializedLogger` component to the registry
```haskell
newInitializedLogger :: IO (Logger IO)
newInitializedLogger = do
  print ("start the logger" :: Text)
  pure (Logger putStrLn putStrLn)
```
Not great, when running the application we print `start the logger` 3 times because the `Logger` is used by 3 other components.

2. use the `memoize @IO @(Logger IO) registry` function to modify the registry.
    This returns `IO (Registry _ _)` because we keep some state to memoize the initialization of the `Logger`
3. run the application and observe that the `Logger` is only initialized once
4. use the `memoizeAll @IO` function to memoize all the components at once and make sure you don't forget any

### Exercise 8 (advanced)

By default a registry will collect all the types of values and functions that you register.
The type-level lists tracked by a registry can grow quite large.

You can reduce those list by using:

 - `eraseTypes` to return a registry with type `Registry [ERASED_TYPES] [ERASED_TYPES]`
 - `normalize` to de-duplicate types in each list
