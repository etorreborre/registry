{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Test.Data.Registry.Internal.RegistrySpec where

import           Data.Dynamic
import           Data.Registry
import           Data.Registry.Internal.Registry
import qualified Hedgehog.Gen                    as Gen
import qualified Hedgehog.Range                  as Range
import           Protolude                       as P
import           Test.Tasty.Extensions
import           Type.Reflection

test_find_value = prop "find a value in a list of values when there are no specializations" $ do
  values <- forAll $ gen @Values
  context <- forAll $ gen @Context
  let value = 1 :: Int
  let values' = toUntyped (val value) `addValue` values
  (fromDynamic <$> findValue (dynTypeRepOf (val value)) context mempty values') === Just (Just value)

registry =
     fun   (genList @Untyped)
  +: fun   (genList @SomeTypeRep)
  +: fun   genUntyped
  +: fun   genSomeTypeRep
  +: pureM @Gen Values
  +: pureM @Gen Context
  +: pureM @Gen Functions
  +: end

genSomeTypeRep :: Gen Untyped -> Gen SomeTypeRep
genSomeTypeRep genValue = do
  Untyped a _ <- genValue
  pure $ dynTypeRep a

genUntyped :: Gen Untyped
genUntyped = Gen.element [toUntyped $ val (1 :: Int), toUntyped $ val (2 :: Int), toUntyped $ val ("1" :: Text)]

genList :: forall a . Gen a -> Gen [a]
genList = Gen.list (Range.linear 1 3)

gen :: forall a . (Typeable a) => Gen a
gen = makeUnsafe registry
----
tests = $(testGroupGenerator)
