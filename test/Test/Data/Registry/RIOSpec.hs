{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Test.Data.Registry.RIOSpec where

import           Data.IORef
import           Data.Registry
import           Protolude             as P
import           Test.Tasty.Extensions

test_close_resource = prop "RIO resources must be closed, with warmup" $ do
  ref <- liftIO $ newIORef []
  let rio = do
        ref' <- allocate (pure ref) (\ref' -> modifyIORef ref' (<>["close"]))
        warmupWith (warmupOf ("test"::Text) $ modifyIORef ref (<>["start"]))
        pure ref'

  res <- liftIO $ withRIO rio $ \ref' -> modifyIORef ref' (<>["use"])
  content <- liftIO $ readIORef ref

  isSuccess res === True
  content === ["start", "use", "close" :: Text]

test_close_resource_no_warmup = prop "RIO resources must be closed" $ do

  let rio = do
        ref <- allocate (newIORef []) (\ref -> modifyIORef ref (<>["close"]))
        liftIO $ modifyIORef ref (<>["start"])
        pure ref

  ref <- liftIO $ withNoWarmupRIO rio $ \ref -> modifyIORef ref (<>["use"]) $> ref
  content <- liftIO $ readIORef ref
  content === ["start", "use", "close" :: Text]

-- withNoWarmupRIO :: RIO a -> (a -> IO b) -> IO b

-- withRIO :: RIO a -> (a -> IO ()) -> IO Result

-- withRegistry :: forall a b ins out . (Typeable a, Contains (RIO a) out, Solvable ins out) =>
--      Registry ins out
--   -> (Result -> a -> IO b)
--   -> IO b

-- runRegistryT :: forall a ins out . (Typeable a, Contains (RIO a) out, Solvable ins out) => Registry ins out -> ResourceT IO (a, Warmup)
-- executeRegistry :: forall a ins out . (Typeable a, Contains (RIO a) out, Solvable ins out) => Registry ins out -> IO (a, Warmup, Stop)
-- unsafeRun :: forall a ins out . (Typeable a, Contains (RIO a) out) => Registry ins out -> IO a
-- unsafeRunDynamic :: forall a ins out . (Typeable a) => Registry ins out -> IO a
-- unsafeRunWithStop :: forall a ins out . (Typeable a, Contains (RIO a) out) => Registry ins out -> IO (a, Stop)
-- unsafeRunDynamicWithStop :: forall a ins out . (Typeable a) => Registry ins out -> IO (a, Stop)
-- warmupWith :: Warmup -> RIO ()
-- allocate :: IO a -> (a -> IO ()) -> RIO a

tests = $(testGroupGenerator)
