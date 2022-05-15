# Applications

There are many ways to structure Haskell applications, the [Handle pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html), the [ReaderT pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern), [mtl](https://ocharles.org.uk/blog/posts/2016-01-26-transformers-free-monads-mtl-laws.html), [free monads](http://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html). Some approaches rely more on the type system than others, for example the `mtl` way [benefits a lot](http://www.parsonsmatt.org/2018/04/10/transforming_transformers.html) from the `DerivingVia` language extension.

The approach we present here complements the "Handle pattern". It uses a Registry to support all the functionalities we might expect when building applications. We generally want to:

 1. [define independent components](#define-components) requiring only knowledge of their immediate dependencies
 1. [unit test](#unit-test) them
 1. [configure](#configuration) them for different environments
 1. [instantiate the full application](#make-the-application) or a subset of it
 1. [integrate the application](#integration) and mock some dependencies
 1. [manage resources](#resources)
 1. [memoize effectful creations](#memoization)
 1. [define context-dependent configurations](#context-dependent-configurations)
 1. [parametrize components](#parametrize-components-with-a-monad) with a specific monad

#### GoodBookings.com

We are going to build a booking application which needs to:

 1. listen to reservation requests ("bookings") and store them in a database
 1. listen to accommodation availabilities and store them in a database
 1. offer an API to browse bookings, availabilities and make a manual match
 1. support logging for debugging, auditing,...

### Define components

A modular approach to building such an application consists in defining distinct components:

`Logger.hs`
```haskell
module Logger where

data Logger = Logger {
  info  :: Text -> IO ()
, error :: Text -> IO ()
}

newLogger :: Logger
newLogger = Logger {
  info t  = print ("[INFO] " <> t)
, error t = print ("[ERROR] " <> t)
}
```

`Database.hs`
```haskell
-- low level sql interactions with a database
module Database where

data Database = Database {
  get    :: (FromRow a) => Command -> IO (Maybe a)
, list   :: (FromRow a) => Command -> IO [a]
, insert :: (ToRow a)   => Command -> [a] -> IO ()
}

data DatabaseConfig = Config {
  host :: Text
, port :: Int
}

-- Starting the database is likely to be an IO action
newDatabase :: DatabaseConfig -> Logger -> IO Database
newDatabase = ...
```
`BookingRepository.hs`
```haskell
module BookingRepository where

-- A "domain level" component for storing and retrieving all the data
data BookingRepository = BookingRepository {
  storeRequest   :: Request -> IO ()
, getRequestById :: RequestId -> IO (Maybe Request)
, getAllRequests :: IO [Request]
}
-- + similar code for availabilities and confirmations

newBookingRepository :: BookingRepositoryConfig -> Logger -> Database -> BookingRepository
newBookingRepository = ...

```
`EventListener.hs`
```haskell
module EventListener where

-- A generic event listener to an events system like Kafka
data EventListener = EventListener {
  -- subscribe to events and consume them
  consumeEvents :: ([Event] -> IO ()) -> IO ()
}

data EventListenerConfig = Config {
  eventTopic :: URI
}

newEventListener :: EventListenerConfig -> EventListener
newEventListener = ...
```
`BookingEventListener.hs`
```haskell
module BookingEventListener where

-- \ A specific listener for bookings
data BookingEventListener = BookingEventListener {
  consumeBookings :: IO ()
}

newBookingEventListener :: Logger -> EventListener -> BookingEventListener
newBookingEventListener = ...
```
`AvailabilitiesEventListener.hs`
```haskell
module AvailabilitiesEventListener where

-- A specific listener for Availabilities
data AvailabilitiesEventListener = AvailabilitiesEventListener {
  consumeAvailabilities :: IO ()
}

newAvailabilitiesEventListener :: Logger -> EventListener -> AvailabilitiesEventListener
newAvailabilitiesEventListener = ...
```
`Api.hs`
```haskell
module Api where

-- A HTTP API to query the data in the database
data Api = Api {
  getBookings       :: Request -> IO Response
, getAvailabilities :: Request -> IO Response
, createMatch       :: Request -> IO Response
}

newApi :: Logger -> BookingRepository -> AvailabilitiesRepository -> Api
newApi = ...
```
`App.hs`
```haskell
module App where

data App = App {
  api            :: Api
, bookings       :: BookingsEventListener
, availabilities :: AvailabilitiesEventListener
}

newApp
  :: Logger
  -> Api
  -> BookingsEventListener
  -> AvailabilitiesEventListener
  -> App
newApp = ...
```

The overall dependency graph looks like
```
                         App

       +------------------+----------------------+
       |                  |                      |
       v                  v                      v
      Api  BookingsEventListener AvailabilitiesEventListener
       |                  |                      |
       |                  |                      |
       v                  |                      |
  BookingRepository <-----+----------------------+
       |
       v
    Database
```

On this diagram we don't show the `Logger` component which is likely to be embedded everywhere and the `EventListener` component embedded in both `Bookings` and `Availabilities` listeners.

### Unit test

Unit-testing components as defined above is really straightforward because each component defines a `new` function for which you can provide dummy values if necessary. For example a `Logger` which doesn't print anything to the console:
```haskell
noLogger = Logger {
  info  = const (pure ())
, error = const (pure ())
}
```

### Configuration

Configuring the application consists in gathering all constructors (the `newXXX` functions) and the required pieces of configuration (the `Config` datatypes) into a `Registry`:
```haskell
registry =
     fun newApp
  <: fun newBookingEventListener
  <: fun newAvailabilitiesEventListener
  <: fun newApi
  <: fun newEventListener
  <: fun newBookingRepository
  <: fun newDatabase
  <: val (EventListenerConfig [uri|https://kafka-prod/bookings])
  <: val (DatabaseConfig "postgres://database-prod" 5432)
```

The code above creates a `Registry` by adding values and constructors with the `<:` operator. `val` signifies that this is a value which can be shown (it has a `Show` instance) and `fun` signifies that this is a "constructor" for which we can only display the type. This is useful when you want to display the configuration of your application at startup.

Note that each time you use the `<:` operator to add a function to the registry the compiler is going to check if there is a way to make
all the function inputs from existing functions in the registry.

It is also possible to "override" some functions or values by adding more functions or values on top of an existing registry.
In that those functions and values will be used first when making components. For example we change the configuration used by the database
by adding a different database configuration on top of the production registry:
```haskell
dev =
     val (DatabaseConfig "localhost" 5432)
  <: prod
```

Then we can make the application with registry.

### Make the application

In order to make the application we use the `TypeApplications` language extension and the `Data.Registry.make` function:
```haskell
app :: App
app = make @App devRegistry
```

`make` will recursively build all the dependencies until it can build the full `App`.

We don't have to build the whole application. Once we have a registry we can also integrate subsets of the application.

### Integration

For example we can just as easily instantiate and start the `BookingEventListener`:
```haskell
listener = make @BookingEventListener devRegistry

consumeBookings listener
```
This will create both the listener and the underlying database so that consumed events will be stored.
Yet, for integration testing you might prefer to skip storing bookings altogether. In that case you can define a `MockDatabase`:
```haskell
mockDatabase = Database {
  get = pure Nothing
, list = pure []
, insert = pure ()
}
```

And add it "on top" of the `devRegistry`:
```haskell
listener = make @BookingEventListener (fun mockDatabase <: devRegistry)

consumeBookings listener
```
Now no booking will be stored in the database while you run the listener.
Can wiring and re-wiring your application be simpler than that?

Actually the full story is a bit more complicated :-).
For one thing some components need to carefully allocate resources.

### Resources

For example the constructor for the `Database` returns an `IO Database`. This is problematic for the registry resolution algorithm because the `newBookingRepository` function requires a `Database` not an `IO Database`. What can we do? The simplest thing is to actually "lift" everything into the same `IO` monad using some variations of the `val` and `fun` combinators:

 - `valTo @m`    lifts a value `a` into `m a`
 - `funTo @m`    lifts a function `a -> b -> c -> ... -> o` into `m a -> m b -> m c -> ... -> m o`

(please read the [reference guide](./reference.md) for a list of the "lifting" combinators)

This means that a "real-life" application registry looks like:
```haskell
registry =
     funTo @RIO newApp
  <: funTo @RIO newBookingEventListener
  <: funTo @RIO newAvailabilitiesEventListener
  <: funTo @RIO newApi
  <: funTo @RIO newEventListener
  <: funTo @RIO newBookingRepository
  <: funTo @RIO newDatabase
  <: valTo @RIO (EventListenerConfig [uri|https://kafka/bookings])
  <: valTo @RIO (DatabaseConfig "postgres://database" 5432)

-- | This type alias is available in the `Data.Registry.RIO` module
type RIO a = ResourceT IO a
```

In general we use a `ResourceT` monad because components allocating resources should better close them down gracefully when they are done.
Once you've settled on a monad to handle resources you can make your registry prettier:
```haskell
registry =
     addFun newApp
  <: addFun newBookingEventListener
  <: addFun newAvailabilitiesEventListener
  <: addFun newApi
  <: addFun newEventListener
  <: addFun newBookingRepository
  <: addFun newDatabase
  <: addVal (EventListenerConfig [uri|https://kafka/bookings])
  <: addVal (DatabaseConfig "postgres://database" 5432)

addFun = funTo @RIO
addVal = valTo @RIO
```

In terms of resources management we are almost there. We still need to solve one issue.

When we use `newDatabase` we get back an `IO Database` which goes on top of the stack. This `IO Database` value can then be used by all the lifted functions in our registry, like `newBookingRepository`. However since the `BookingRepository` is used by 3 other components every time we use it we will get a new version of the `Database`! Because the action `IO Database` will be executed 3 times giving us 3 accesses to the database. This is clearly undesirable since a `Database` component maintains a pool of connections to the database. This actually goes for **any** effect you run in constructor, like printing some status in the logs on startup!

What we need is to "memoize" the creation of the database.

### Memoization

The `memoize` function does exactly this:
```haskell
registry =
     memoize @IO @Database $
     addFun newApp
  <: addFun newBookingEventListener
  <: addFun newAvailabilitiesEventListener
  <: addFun newApi
  <: addFun newEventListener
  <: addFun newBookingRepository
  <: addFun newDatabase
  <: addVal (EventListenerConfig [uri|https://kafka/bookings])
  <: addVal (DatabaseConfig "postgres://database" 5432)
```

The `memoize` declaration will slightly modify the registry to say "if you create an `IO Database` cache this action so that the same `Database` is returned every time an `IO Database` value is needed. Since caching is involved the signature of the `registry` changes from a pure value to a monadic one:
```haskell
registry :: IO (Registry inputs outputs)
registry = memoize @IO @Database devRegistry
```
And if you need to memoize the creation of several components in your application you will have to use the "bind" monadic operator
```haskell
registry :: IO (Registry inputs outputs)
registry =
  memoize @IO @Database devRegistry >>=
  memoize @IO @Metrics
```

Since it can be a bit tedious to write all those declarations, there is a function `memoizeAll @m` which goes through the whole list of "output types" in the registry, of the form `m a` and which invokes the specific `memoize @m @a` function. Even better, if you use the `Data.Registry.RIO.withRegistry` function to use your registry, the `memoize` function is automatically called so that you won't have to worry about running too many side-effects.

One last difficulty needs to be addressed now.

We have 2 different listeners which are both using an `EventListener`. That component can be configured to listen to a specific queue of events with `EventListenerConfig`. But if the 2 listeners eventually share the same configuration they are going to listen to the same event!

### Context dependent configurations

What we need then is to "specialize" the configuration to use depending on what we are building. If we are building a `BookingEventListener` we want the `EventListener` to be created with `configBooking` and if we are building an `AvailabilityEventListener` we want the `EventListener` to be created with `configAvailability`
```haskell
configBooking =
  EventListenerConfig [uri|https://kafka-prod/bookings]

configAvailability =
  EventListenerConfig [uri|https://kafka-prod/availabilities]
```

Then we need to tell the Registry what we want to happen with the `specialize` function:
```haskell
registry =
-- when trying to build IO BookingEventListener, use configBooking whenever
-- an EventListenerConfig is required
  specializeVal @(IO BookingEventListener) configBooking .
  specializeVal @(IO AvailabilityEventListener) configAvailability $
  devRegistry
```

If it all looks too confusing please have a look at the [reference guide](./reference.md) to see all the available combinators and their meaning at once.

### Parametrize components with a monad

It can be useful to make the interfaces to your components slightly more generic in terms of what "effects" the interfaces
can offer. For example `Logger` could be implemented like this:

```haskell
data Logger m = Logger {
  info  :: Text -> m ()
, error :: Text -> m ()
}

-- | TraceId can be used to add some "tracing" information to the log
--   statements
new :: (MonadReader TraceId, MonadIO m) => Logger m
new = Logger m {
  info t  = print ("[INFO] " <> t)
, error t = print ("[ERROR] " <> t)
}
```

Then when you define your registry you can specify which monad `m` you want your components to run in.
For example:
```haskell

type TIO = ReaderT TraceId IO

components =
     addFun newApp
  <: addFun (newBookingEventListener @TIO)
  <: addFun (newAvailabilitiesEventListener @TIO)
  <: addFun (newEventListener @TIO)
  <: addFun (newApi @TIO)
  <: addFun (newBookingRepository @TIO)
  <: addFun (newDatabase @TIO)
  <: addVal (EventListenerConfig [uri|https://kafka/bookings])
  <: addVal (DatabaseConfig "postgres://database" 5432)
```

The monad you use should be more or less isomorphic to `ReaderT Env IO` to allow your components to access `IO` directly
and benefit from libraries for [async computations](https://hackage.haskell.org/package/async), [retrying](https://hackage.haskell.org/package/retry),
or [controlling resources](https://hackage.haskell.org/package/resourcet).
