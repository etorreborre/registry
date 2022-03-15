# Avoiding "parameter-passing"

One downside of using explicit components to implement a given behaviour is that we have to pass those components down to every place where they are used.
We can avoid doing that with 2 simple tricks, `RecordWildCards` and `where`

Let's say we have a `Service` using 2 components, a `Logger` and a `Tracer`:
```
{-# LANGUAGE RecordWildCards #-}

data Logger m = Logger {
  -- a function taking 2 arguments
  info  :: Text -> Text -> m ()
  -- a function having a constraint
, error :: forall a . Monoid a => a -> Text -> m ()
}

newtype Tracer m = Tracer {
  traceIt :: Text -> m ()
}

newtype Service m = Service {
  doIt :: Int -> Text -> m ()
}

newService :: Monad m => Logger m -> Tracer m -> Service m
newService logger tracer = Service {..}
  where
    doIt :: Int -> Text -> m ()
    doIt n t = do
      info logger "doing it" t
      traceIt tracer (show n)
```

`logger` and `tracer` being in scope of `where` they can be accessed to implement `doIt`. The main drawbacks of this approach are:

 - the need for one level of indentation for the code of a component (not very different from the code in a class of an OO language if you think about it)
 - the need to use the component names, `logger` and `tracer` instead of just calling `info` and `traceIt`
 - the obligation to build a component with all its dependencies in order to access just one function
