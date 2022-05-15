{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Internal.CacheSpec where

import Control.Concurrent.Async
import Data.Registry.Internal.Cache
import Protolude as P
import Test.Tasty.Extensions

test_cache = test "caching an IO action must always return the same value" $ do
  cached <- liftIO $ do
    -- create an action which will increment an Int everytime it is called
    ref <- newMVar (0 :: Int)
    let action = modifyMVar_ ref (pure . (+ 1)) >> readMVar ref
    cache <- newCache

    -- when the action is cached it will always return the same value
    let cachedAction = fetch cache Nothing action
    void $ replicateConcurrently_ 100 cachedAction -- with concurrent accesses
    cachedAction

  cached === 1
