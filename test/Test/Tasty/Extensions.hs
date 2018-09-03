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
module Test.Tasty.Extensions (
  module H
, gotException
, prop
, test
, minTestsOk
) where

import           GHC.Stack
import           Hedgehog            as H hiding (test)
import           Hedgehog.Corpus     as H
import           Hedgehog.Gen        as H hiding (discard, print)
import           Protolude           hiding ((.&.))
import           Test.Tasty
import           Test.Tasty.Hedgehog

-- | Create a Tasty test from a Hedgehog property
prop :: HasCallStack => TestName -> PropertyT IO () -> [TestTree]
prop name p = [withFrozenCallStack $ testProperty name (H.property p)]

-- | Create a Tasty test from a Hedgehog property called only once
test :: HasCallStack => TestName -> PropertyT IO () -> [TestTree]
test name p = withFrozenCallStack $
  minTestsOk 1  . localOption (HedgehogShrinkLimit (Just (0 :: ShrinkLimit))) <$> prop name p

gotException :: forall a . (HasCallStack, Show a) => a -> PropertyT IO ()
gotException a = withFrozenCallStack $ do
  res <- liftIO (try (evaluate a) :: IO (Either SomeException a))
  case res of
    Left _ -> assert True
    Right _ -> annotateShow "excepted an exception" >> assert False



-- * Parameters

minTestsOk :: Int -> (TestTree -> TestTree)
minTestsOk n = localOption (HedgehogTestLimit (Just (fromInteger (toInteger n))))
