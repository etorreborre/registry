{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE UndecidableInstances  #-}
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

import           Data.Registry.Make
import           Data.Registry.Registry
import           Data.Registry.Solver
import           Data.Registry.Warmup
import           Protolude

-- | Data type encapsulating resource finalizers
newtype Stop = Stop InternalState

-- | Run all finalizers
runStop :: Stop -> IO ()
runStop (Stop is) = runResourceT $ closeInternalState is

-- | This newtype creates a monad to sequence
--   component creation actions, cumulating start/stop tasks
--   found along the way

newtype RioT m a =
  RioT
  { runRioT :: Stop -> m (a, Warmup) }
  deriving (Functor)

-- | Specialization of RioT to IO
type RIO = RioT IO

runRIO :: RIO a -> Stop -> IO (a, Warmup)
runRIO = runRioT

instance (Monad m) => Applicative (RioT m) where
  pure a =
    RioT (const (pure (a, mempty)))

  RioT fab <*> RioT fa =
    RioT $ \s ->
      do (f, sf) <- fab s
         (a, sa) <- fa s
         pure (f a, sf `mappend` sa)

instance (Monad m) => Monad (RioT m) where
  return = pure

  RioT ma >>= f =
    RioT $ \s ->
      do (a, sa) <- ma s
         (b, sb) <- runRioT  (f a) s
         pure (b, sa `mappend` sb)

instance (MonadIO m) => MonadIO (RioT m) where
  liftIO io = RioT (const $ (, mempty) <$> liftIO io)

instance (MonadThrow m) => MonadThrow (RioT m) where
  throwM e = RioT (const $ throwM e)

instance (MonadBase IO m, MonadIO m) => MonadBase IO (RioT m) where
  liftBase = liftIO

instance MonadResource m => MonadResource (RioT m) where
  liftResourceT action = RioT $ \(Stop s) -> liftIO ((, mempty) <$> runInternalState action s)

instance MonadTrans RioT where
  lift :: Monad m => m a -> RioT m a
  lift ma = RioT (const $ (, mempty) <$> ma)

-- * For production

-- | This function must be used to run services involving a top component
--   It creates the top component and invokes all warmup functions
--
--   The passed function 'f' is used to decide whether to continue or
--   not depending on the Result
withRegistry :: forall a b ins out m . (Typeable a, Typeable m, MonadIO m, MonadUnliftIO m, Contains (RioT m a) out, Solvable ins out) =>
     Registry ins out
  -> (Result -> a -> m b)
  -> m b
withRegistry registry f = runResourceT $ do
  (a, warmup) <- runRegistryT @a registry
  result      <- lift . liftIO $ runWarmup warmup
  lift $ f result a

-- | This can be used if you want to insert the component creation inside
--   another action managed with 'ResourceT'. Or if you want to call 'runResourceT' yourself later
runRegistryT :: forall a ins out m . (Typeable a, Typeable m, MonadIO m, Contains (RioT m a) out, Solvable ins out) => Registry ins out -> ResourceT m (a, Warmup)
runRegistryT registry = withInternalState $ \is -> runRioT (make @(RioT m a) registry) (Stop is)

-- * For testing

-- | Instantiate the component but don't execute the warmup (it may take time)
--   and keep the Stop value to clean resources later
--   This function statically checks that the component can be instantiated
executeRegistry :: forall a ins out m . (Typeable a, Typeable m, MonadIO m, Contains (RioT m a) out, Solvable ins out) => Registry ins out -> m (a, Warmup, Stop)
executeRegistry registry = do
  is <- liftIO createInternalState
  (a, w) <- runRioT (make @(RioT m a) registry) (Stop is)
  pure (a, w, Stop is)

-- | Instantiate the component but don't execute the warmup (it may take time) and lose a way to cleanu up resources
-- | Almost no compilation time is spent on checking that component resolution is possible
unsafeRun :: forall a ins out m . (Typeable a, Typeable m, MonadIO m, Contains (RioT m a) out) => Registry ins out -> m a
unsafeRun = unsafeRunDynamic

-- | Instantiate the component but don't execute the warmup (it may take time) and lose a way to cleanu up resources
--   Don't even check that a component can be built out of the registry
unsafeRunDynamic :: forall a ins out m . (Typeable a, Typeable m, MonadIO m) => Registry ins out -> m a
unsafeRunDynamic registry = fst <$> unsafeRunDynamicWithStop registry

-- | Same as 'unsafeRun' but keep the 'Stop' value to be able to clean resources later
unsafeRunWithStop :: forall a ins out m . (Typeable a, Typeable m, MonadIO m, Contains (RioT m a) out) => Registry ins out -> m (a, Stop)
unsafeRunWithStop = unsafeRunDynamicWithStop

unsafeRunDynamicWithStop :: forall a ins out m . (Typeable a, Typeable m, MonadIO m) => Registry ins out -> m (a, Stop)
unsafeRunDynamicWithStop registry = do
  is <- createInternalState
  (a, _) <- runRioT (makeUnsafe @(RioT m a) registry) (Stop is)
  pure (a, Stop is)

-- | Lift a 'Warmup' action into the 'RioT m' monad
warmupWith :: (Applicative m) => Warmup -> RioT m ()
warmupWith w = RioT (const $ pure ((), w))

-- | Allocate some resource
allocate :: (MonadResource m) => IO a -> (a -> IO ()) -> RioT m a
allocate resource cleanup =
  snd <$> Resource.allocate resource cleanup
