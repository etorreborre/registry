{-# OPTIONS_GHC -fno-warn-missing-monadfail-instances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Internal.GensRegistry where

import Data.Dynamic
import Data.List.NonEmpty
import Data.Registry
import Data.Registry.Internal.Types
import Data.Text as T
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Protolude
import Type.Reflection
import Prelude (show)

-- Hedgehog generators for the internal types
gensRegistry =
  funTo @Gen UntypedRegistry
    <: funTo @Gen Context
    -- specializations
    <: funTo @Gen Modifiers
    <: fun (genList @(SomeTypeRep, ModifierFunction))
    <: fun (genPair @SomeTypeRep @ModifierFunction)
    <: fun genModifierFunction
    <: funTo @Gen Specializations
    <: fun (genList @Specialization)
    <: funTo @Gen Specialization
    -- functions
    <: funTo @Gen Functions
    <: fun (genList @Function)
    <: funTo @Gen Function
    <: funTo @Gen FunctionDescription
    -- type reps
    <: fun (genList @(SomeTypeRep, Maybe SomeTypeRep))
    <: fun (genList @SomeTypeRep)
    <: fun (genPair @SomeTypeRep @(Maybe SomeTypeRep))
    <: fun (genPair @(NonEmpty SomeTypeRep) @Value)
    <: fun (genMaybe @SomeTypeRep)
    <: fun (genNonEmpty @SomeTypeRep)
    <: fun genSomeTypeRep
    -- values
    <: funTo @Gen Values
    <: fun (genList @Value)
    <: funTo @Gen ProvidedValue
    <: funTo @Gen ValueDescription
    -- base
    <: fun genDynamic
    <: fun (genList @Text)
    <: fun (genMaybe @Text)
    <: fun genInt
    <: fun genText
    <: fun genTextToInt

-- * generators

newtype TextToInt = TextToInt (Text -> Int)

instance Show TextToInt where show _ = "<function>"

instance Eq TextToInt where _ == _ = True

genTextToInt :: Gen TextToInt
genTextToInt = pure (TextToInt T.length)

data UntypedRegistry = UntypedRegistry
  { _uvalues :: Values,
    _ufunctions :: Functions,
    _uspecializations :: Specializations,
    _umodifiers :: Modifiers
  }
  deriving (Show)

genValues :: Gen (Int, Values)
genValues = do
  value <- genInt
  values <- make @(Gen Values) gensRegistry
  pure (value, createValue value `addValue` values)

genSomeTypeRep :: Gen Value -> Gen SomeTypeRep
genSomeTypeRep = fmap valueDynTypeRep

genDynamic :: Gen Dynamic
genDynamic = Gen.element [toDyn (1 :: Int), toDyn (2 :: Int), toDyn ("1" :: Text)]

genList :: forall a. Gen a -> Gen [a]
genList = Gen.list (Range.linear 1 3)

genNonEmpty :: forall a. Gen a -> Gen (NonEmpty a)
genNonEmpty genA = do
  ls <- Gen.list (Range.linear 1 3) genA
  case ls of
    -- this case can not happen
    [] -> pure <$> genA
    as -> pure (fromList as)

genMaybe :: forall a. Gen a -> Gen (Maybe a)
genMaybe = Gen.maybe

genPair :: forall a b. Gen a -> Gen b -> Gen (a, b)
genPair gena genb = (,) <$> gena <*> genb

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 5)

genText :: Gen Text
genText = Gen.text (Range.linear 2 10) Gen.alphaNum

genModifierFunction :: Gen Function -> Gen ModifierFunction
genModifierFunction genF = do
  f <- genF
  pure (const f)
