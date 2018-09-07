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

test_find_no_value = prop "no value can be found if nothing is stored in the registry" $ do
  value  <- forAll $ gen @Int

  (fromDynamic <$> findValue (dynTypeRepOf (val value)) mempty mempty mempty) === (Nothing :: Maybe (Maybe Int))

test_find_value = prop "find a value in a list of values when there are no specializations" $ do
  (value, values) <- genValues

  (fromDynamic <$> findValue (dynTypeRepOf (val value)) mempty mempty values) === Just (Just value)

test_find_specialized_value = prop "find a value in a list of values when there is a specialization for a given context" $ do
  value <- forAll $ gen @Int
  values <- forAll $ gen @Values
  let listTypeRep = dynTypeRep . toDyn $ [value]
  let context = Context [listTypeRep] -- when trying to build a [Int]
  let specializations = Specializations [(listTypeRep, toDyn value)]

  (fromDynamic <$> findValue (dynTypeRepOf (val value)) context specializations values) === Just (Just value)


genValues = do
  value  <- forAll $ gen @Int
  values <- forAll $ (val value `addTypedValue`) <$> gen @Values
  pure (value, values)

-- * registry for the tests
registry =
     fun   (genList @Untyped)
  +: fun   (genList @SomeTypeRep)
  +: fun   (genList @(SomeTypeRep, Dynamic))
  +: fun   genUntyped
  +: fun   genSomeTypeRep
  +: fun   genDynamic
  +: fun   genInt
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

genDynamic :: Gen Dynamic
genDynamic = Gen.element [toDyn (1 :: Int), toDyn (2 :: Int), toDyn ("1" :: Text)]

genList :: forall a . Gen a -> Gen [a]
genList = Gen.list (Range.linear 1 3)

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 5)

gen :: forall a . (Typeable a) => Gen a
gen = makeUnsafe registry
----
tests = $(testGroupGenerator)
