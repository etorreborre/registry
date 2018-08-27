{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}


{-
  Typeclasses for Modules dependencies resolution.

  The Register typeclass describes a datastructure which
  stores modules

  The Make typeclass describes a way to create a
  module using the current state of a Registry.

  The creation of a module might be effectful
  if some resources need to be allocated.

  The allocation is done with the `allocate` method
  which also specifies how the resources must be cleaned up.

  Creating a module can also return a warmup function
  to control that the module is correctly configured
  (see the Warmup module to learn how to create warmups)

-}
module Data.Box.BIO where

import           Control.Monad.Trans.Resource
import qualified Control.Monad.Trans.Resource as Resource (allocate)

import           Protolude
import           Data.Box.Warmup
import           Data.Box.Make

-- | Data type encapsulating resource finalizers
newtype Stop = Stop InternalState

runStop :: Stop -> IO ()
runStop (Stop is) = runResourceT $ closeInternalState is

-- | This newtype creates a monad to sequence
--   module creation actions, cumulating start/stop tasks
--   found along the way
newtype BIO a =
  BIO
  { runBIO :: Stop -> IO (a, Warmup) }
  deriving (Functor)

instance Applicative BIO where
  pure a =
    BIO (const (pure (a, mempty)))

  BIO fab <*> BIO fa =
    BIO $ \s ->
      do (f, sf) <- fab s
         (a, sa) <- fa s
         pure (f a, sf `mappend` sa)

instance Monad BIO where
  return = pure

  BIO ma >>= f =
    BIO $ \s ->
      do (a, sa) <- ma s
         (b, sb) <- runBIO (f a) s
         pure (b, sa `mappend` sb)

instance MonadIO BIO where
  liftIO io = BIO $ const (liftIO ((\a -> (a, mempty)) <$> io))

instance MonadResource BIO where
  liftResourceT action = BIO $ \(Stop s) -> liftIO ((, mempty) <$> runInternalState action s)

-- * For production

-- | This function must be used to run services involving a top module
--   It creates the top module and invokes all warmup functions
--
--   The passed function 'f' is used to decide whether to continue or
--   not depending on the Result
withBox :: forall a b ins out . (Typeable a, Contains (BIO a) out, Solvable ins out) =>
     Registry ins out
  -> (Result -> a -> IO b)
  -> IO b
withBox registry f = runResourceT $ do
  (a, warmup) <- runBoxT @a registry
  result      <- lift $ runWarmup warmup
  lift $ f result a

-- | This can be used if you want to insert the box creation inside
--   another action managed with ResourceT. Or if you want to call runResourceT yourself later
runBoxT :: forall a ins out . (Typeable a, Contains (BIO a) out, Solvable ins out) => Registry ins out -> ResourceT IO (a, Warmup)
runBoxT registry = withInternalState $ \is -> runBIO (make @(BIO a) registry) (Stop is)

-- * For testing

-- | Instantiate the box but don't execute the warmup (it may take time)
--   and keep the Stop value to clean resources later
--   This function statically checks that the box can be instantiated
executeBox :: forall a ins out . (Typeable a, Contains (BIO a) out, Solvable ins out) => Registry ins out -> IO (a, Warmup, Stop)
executeBox registry = do
  is <- createInternalState
  (a, w) <- runBIO (make @(BIO a) registry) (Stop is)
  pure (a, w, Stop is)

-- | Instantiate the box but don't execute the warmup (it may take time) and lose a way to cleanu up resources
-- | Almost no compilation time is spent on checking that box resolution is possible
unsafeRun :: forall a ins out . (Typeable a, Contains (BIO a) out) => Registry ins out -> IO a
unsafeRun registry = fst <$> unsafeRunWithStop registry

-- | Same as unsafeRun but keep the Stop value to be able to clean resources later
unsafeRunWithStop :: forall a ins out . (Typeable a, Contains (BIO a) out) => Registry ins out -> IO (a, Stop)
unsafeRunWithStop registry = do
  is <- createInternalState
  (a, _) <- runBIO (makeUnsafe @(BIO a) registry) (Stop is)
  pure (a, Stop is)

-- * Module creation

warmupWith :: Warmup -> BIO ()
warmupWith w = BIO (const $ pure ((), w))

allocate :: IO a -> (a -> IO ()) -> BIO a
allocate resource cleanup =
  snd <$> Resource.allocate resource cleanup

liftBIO :: IO a -> BIO a
liftBIO io = BIO (\_ -> (, mempty) <$> io)
