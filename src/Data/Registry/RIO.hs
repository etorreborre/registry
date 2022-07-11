-- | Utilities for working with ResourceT IO
module Data.Registry.RIO where

import Control.Monad.Trans.Resource
import Data.Registry.Make
import Data.Registry.Registry
import Data.Registry.Solver
import Protolude

-- | Type alias for ResourceT IO
type RIO = ResourceT IO

-- | This function must be used to run services involving a top component
--   It creates an application of type a and which can return a result of type b.
--
--   We also make sure that all effects are memoized by calling `memoizeAll` on the Registry here!
withRegistry ::
  forall a b ins out m.
  (Typeable a, Contains (RIO a) out, Solvable ins out, MonadIO m, MemoizedActions out) =>
  Registry ins out ->
  (a -> IO b) ->
  m b
withRegistry registry f = liftIO $
  runResourceT (runRegistryT @a registry >>= liftIO . f)

-- | This can be used if you want to insert the component creation inside
--   another action managed with ResourceT. Or if you want to call runResourceT yourself later
runRegistryT ::
  forall a ins out .
  (Typeable a, Contains (RIO a) out, Solvable ins out, MemoizedActions out) =>
  Registry ins out ->
  ResourceT IO a
runRegistryT registry =
  liftIO (memoizeAll @RIO registry) >>= make @(RIO a)
