{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}
{- |

  RIO is equivalent to @ResourceT (WriterT Warmup IO)@
  It can be used to instantiate "components as records of functions"
  where each component can allocate resources and have a "warmup phase"
  to preload data or assess if it is working properly.

-}
module Data.Registry.RIO where

import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import qualified Control.Monad.Trans.Resource as Resource (allocate)

import           Control.Applicative
import           Data.Functor.Alt
import           Data.Registry.Make
import           Data.Registry.Registry
import           Data.Registry.Solver
import           Data.Registry.Warmup
import           Protolude                    hiding (Alt, try)

-- | Data type encapsulating resource finalizers
newtype Stop = Stop InternalState

-- | Run all finalizers
runStop :: Stop -> IO ()
runStop (Stop is) = runResourceT $ closeInternalState is

-- | This newtype creates a monad to sequence
--   component creation actions, cumulating start/stop tasks
--   found along the way

newtype RIO a = RIO { runRIO :: Stop -> IO (a, Warmup) } deriving (Functor)

instance Applicative RIO where
  pure a =
    RIO (const (pure (a, mempty)))

  RIO fab <*> RIO fa =
    RIO $ \s ->
      do (f, sf) <- fab s
         (a, sa) <- fa s
         pure (f a, sf `mappend` sa)

instance Monad RIO where
  return = pure

  RIO ma >>= f =
    RIO $ \s ->
      do (a, sa) <- ma s
         (b, sb) <- runRIO  (f a) s
         pure (b, sa `mappend` sb)

instance MonadIO RIO where
  liftIO io = RIO (const $ (, mempty) <$> liftIO io)

instance MonadThrow RIO where
  throwM e = RIO (const $ throwM e)

instance MonadBase IO RIO where
  liftBase = liftIO

instance MonadResource RIO where
  liftResourceT action = RIO $ \(Stop s) -> liftIO ((, mempty) <$> runInternalState action s)

-- We cannot piggy-back on the IO Alternative instance
-- because it only catches IOErrors
instance Alternative RIO where
  empty = RIO (const empty)
  (RIO runA) <|> (RIO runB) = RIO $ \s -> do
    res <- try (runA s)
    case res of
      Left (_::SomeException) -> runB s
      Right r                 -> pure r

instance Alt RIO where
  (<!>) = (<|>)

-- * For production

-- | Use a RIO value and make sure that resources are closed
--   Only run the action if the warmup is successful
withRIO :: (MonadIO m) => RIO a -> (a -> IO ()) -> m Result
withRIO rio f = liftIO $ runResourceT $ withInternalState $ \is ->
  do  (a, warmup) <- runRIO rio (Stop is)
      result      <- liftIO $ runWarmup warmup
      if isSuccess result then f a else pure ()
      pure result

-- | This function must be used to run services involving a top component
--   It creates the top component and invokes all warmup functions
--
--   The passed function 'f' is used to decide whether to continue or
--   not depending on the Result
--
--   We also make sure that all effects are memoized by calling `singletons` on the Registry here!
withRegistry :: forall a b ins out m . (Typeable a, Contains (RIO a) out, Solvable ins out, MonadIO m, ToSingletons out) =>
     Registry ins out
  -> (Result -> a -> IO b)
  -> m b
withRegistry registry f = liftIO $ runResourceT $ do
  (a, warmup) <- runRegistryT @a registry
  result      <- lift . liftIO $ runWarmup warmup
  lift $ f result a

-- | This can be used if you want to insert the component creation inside
--   another action managed with 'ResourceT'. Or if you want to call 'runResourceT' yourself later
runRegistryT :: forall a ins out m . (Typeable a, Contains (RIO a) out, Solvable ins out, MonadIO m, ToSingletons out)
  => Registry ins out
  -> ResourceT m (a, Warmup)
runRegistryT registry = withInternalState $ \is -> do
  r <- liftIO $ singletons @RIO registry
  liftIO $ runRIO (make @(RIO a) r) (Stop is)

-- * For testing

-- | Use a RIO value and make sure that resources are closed
--   Don't run the warmup
withNoWarmupRIO :: (MonadIO m) => RIO a -> (a -> IO b) -> m b
withNoWarmupRIO rio f = liftIO $
  runResourceT $ withInternalState $ \is ->
  f . fst =<< runRIO rio (Stop is)

-- | Use a RIO value and make sure that resources are closed
--   Run the warmup but ignore the result
withRIOIgnoreWarmupResult :: (MonadIO m) => RIO a -> (a -> IO b) -> m b
withRIOIgnoreWarmupResult = withRIOAndWarmupResult (const $ pure ())

-- | Use a RIO value and make sure that resources are closed
--   Run a unit function with the warmup result (print or throw exception)
withRIOAndWarmupResult :: (MonadIO m) => (Result -> IO ()) -> RIO a -> (a -> IO b) -> m b
withRIOAndWarmupResult withResult rio f = liftIO $
  runResourceT $ withInternalState $ \is -> do
    (a, warmup) <- runRIO rio (Stop is)
    warmupResult <- liftIO $ runWarmup warmup
    withResult warmupResult
    liftIO (f a)

-- | Instantiate the component but don't execute the warmup (it may take time)
--   and keep the Stop value to clean resources later
--   This function statically checks that the component can be instantiated
executeRegistry :: forall a ins out m . (Typeable a, Contains (RIO a) out, Solvable ins out, MonadIO m) => Registry ins out -> m (a, Warmup, Stop)
executeRegistry registry = liftIO $ do
  is <- liftIO createInternalState
  (a, w) <- runRIO (make @(RIO a) registry) (Stop is)
  pure (a, w, Stop is)


-- | Instantiate the component but don't execute the warmup (it may take time) and lose a way to cleanu up resources
-- | Almost no compilation time is spent on checking that component resolution is possible
unsafeRun :: forall a ins out m . (Typeable a, Contains (RIO a) out, MonadIO m) => Registry ins out -> m a
unsafeRun = unsafeRunDynamic

-- | Instantiate the component but don't execute the warmup (it may take time) and lose a way to cleanu up resources
--   Don't even check that a component can be built out of the registry
unsafeRunDynamic :: forall a ins out m . (Typeable a, MonadIO m) => Registry ins out -> m a
unsafeRunDynamic registry = liftIO $ fst <$> unsafeRunDynamicWithStop registry

-- | Same as 'unsafeRun' but keep the 'Stop' value to be able to clean resources later
unsafeRunWithStop :: forall a ins out m . (Typeable a, Contains (RIO a) out, MonadIO m) => Registry ins out -> m (a, Stop)
unsafeRunWithStop = unsafeRunDynamicWithStop

unsafeRunDynamicWithStop :: forall a ins out m . (Typeable a, MonadIO m) => Registry ins out -> m (a, Stop)
unsafeRunDynamicWithStop registry = liftIO $ do
  is <- createInternalState
  (a, _) <- runRIO (makeUnsafe @(RIO a) registry) (Stop is)
  pure (a, Stop is)

-- | Lift a 'Warmup' action into the 'RIO` monad
warmupWith :: Warmup -> RIO ()
warmupWith w = RIO (const $ pure ((), w))

-- | Allocate some resource
allocate :: IO a -> (a -> IO ()) -> RIO a
allocate resource cleanup =
  snd <$> Resource.allocate resource cleanup
