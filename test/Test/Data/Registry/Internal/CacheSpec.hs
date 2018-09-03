{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Internal.CacheSpec where

import           Control.Concurrent.Async
import           Data.IORef
import           Data.Registry.Internal.Cache
import           Hedgehog                     ((===))
import           Protolude                    as P
import           Test.Tasty
import           Test.Tasty.Extensions
import           Test.Tasty.TH

test_cache = test "caching an IO action must always return the same value" $ do
  cached <- liftIO $ do
    -- create an action which will increment an Int everytime it is called
    ref <- newIORef (0 :: Int)
    let action = modifyIORef ref (+1) >> readIORef ref
    cache <- newCache

    -- when the action is cached it will always return the same value
    let cachedAction = fetch cache action
    _ <- replicateConcurrently_ 100 cachedAction -- with concurrent accesses
    cachedAction

  cached === 1

----
tests = $(testGroupGenerator)
