{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Test.Data.Registry.Internal.RegistrySpec where

import           Data.Dynamic
import           Data.Registry
import           Data.Registry.Internal.Registry
import           Protolude                        as P hiding (show)
import           Test.Data.Registry.Internal.Gens
import           Test.Tasty.Extensions

test_find_no_value = prop "no value can be found if nothing is stored in the registry" $ do
  value  <- forAll $ gen @Int

  (fromDynamic <$> findValue (dynTypeRepOf (val value)) mempty mempty mempty) === (Nothing :: Maybe (Maybe Int))

test_find_value = prop "find a value in a list of values when there are no specializations" $ do
  (value, values) <- forAll genValues

  (fromDynamic <$> findValue (dynTypeRepOf (val value)) mempty mempty values) === Just (Just value)

test_find_specialized_value = prop "find a value in a list of values when there is a specialization for a given context" $ do
  value <- forAll $ gen @Int
  values <- forAll $ gen @Values
  let listTypeRep = dynTypeRep . toDyn $ [value]
  let context = Context [listTypeRep] -- when trying to build a [Int]
  let specializations = Specializations [(listTypeRep, toDyn value)]

  (fromDynamic <$> findValue (dynTypeRepOf (val value)) context specializations values) === Just (Just value)

test_find_no_constructor = prop "no constructor can be found if nothing is stored in the registry" $ do
  value  <- forAll $ gen @Int

  (fromDynamic <$> findConstructor (dynTypeRepOf (val value)) mempty) === (Nothing :: Maybe (Maybe Int))

test_find_contructor = prop "find a constructor in a list of constructors" $ do
  (Function function) <- forAll $ gen @Function
  functions <- forAll $ (fun function `addTypedFunction`) <$> gen @Functions

  let outputType = dynTypeRepOf (val (1 :: Int))

  (fmap Function <$> (fromDynamic <$> findConstructor outputType functions)) ===
    Just (Just (Function function))

test_store_value_no_modifiers = prop "a value can be stored in the list of values" $ do
  (value, values) <- forAll genValues

  let (Right stored) = execStateT (storeValue mempty (toDyn value)) values
  let found = findValue (dynTypeRep . toDyn $ value) mempty mempty stored
  (fromDynamic <$> found) === Just (Just value)

test_store_value_with_modifiers = prop "a value can be stored in the list of values but modified beforehand" $ do
  (value, values) <- forAll genValues

  let valueType = dynTypeRep . toDyn $ value
  let modifiers = Modifiers [(valueType, toDyn (\(i:: Int) -> i + 1))]
  let (Right stored) = execStateT (storeValue modifiers (toDyn value)) values

  let found = findValue valueType mempty mempty stored
  (fromDynamic <$> found) === Just (Just (value + 1))

test_store_value_ordered_modifiers = prop "modifiers are applied in a LIFO order" $ do
  (value, values) <- forAll genValues

  let valueType = dynTypeRep . toDyn $ value
  let modifiers = Modifiers [
         (valueType, toDyn (\(i:: Int) -> i * 2))
       , (valueType, toDyn (\(i:: Int) -> i + 1))
       ]
  let (Right stored) = execStateT (storeValue modifiers (toDyn value)) values

  let found = findValue valueType mempty mempty stored
  (fromDynamic <$> found) === Just (Just ((value * 2) + 1))

----
tests = $(testGroupGenerator)
