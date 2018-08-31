{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-|

Registry      : Test.Tasty.Extensions
Description : Tasty / Hedgehog / HUnit integration

This module unifies property based testing with Hedgehog
and one-off tests.

-}
module Test.Tasty.Extensions where

import           GHC.Stack
import           Hedgehog            as HH
import           Protolude           hiding ((.&.))
import           Test.Tasty
import           Test.Tasty.Hedgehog

-- | Create a Tasty test from a Hedgehog property
prop :: HasCallStack => TestName -> PropertyT IO () -> [TestTree]
prop name p = [withFrozenCallStack $ testProperty name (HH.property p)]

-- | Create a Tasty test from a Hedgehog property called only once
test :: HasCallStack => TestName -> PropertyT IO () -> [TestTree]
test name p = withFrozenCallStack $
  (minTestsOk 1  . localOption (HedgehogShrinkLimit (Just (0 :: ShrinkLimit)))) <$> prop name p

-- * Parameters

minTestsOk :: Int -> (TestTree -> TestTree)
minTestsOk n = localOption (HedgehogTestLimit (Just (fromInteger (toInteger n))))
