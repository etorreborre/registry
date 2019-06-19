{-# OPTIONS_GHC -fno-warn-missing-monadfail-instances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Internal.GensRegistry where

import           Data.Dynamic
import           Data.List.NonEmpty
import           Data.Registry
import           Data.Registry.Internal.Types
import           Data.Text                    as T
import           Hedgehog
import           Hedgehog.Gen                 as Gen
import           Hedgehog.Range               as Range
import           Prelude                      (show)
import           Protolude
import           Type.Reflection

-- Hedgehog generators for the internal types
gensRegistry =
     funTo @Gen UntypedRegistry
  +: funTo @Gen Values
  +: funTo @Gen Functions
  +: fun genModifierFunction
  +: funTo @Gen Specializations
  +: funTo @Gen Modifiers
  +: funTo @Gen Context
  +: funTo @Gen Function
  +: funTo @Gen ProvidedValue
  +: funTo @Gen ValueDescription
  +: funTo @Gen FunctionDescription
  +: funTo @Gen Specialization
  +: fun   (genNonEmpty @SomeTypeRep)
  +: fun   (genList @Specialization)
  +: fun   (genList @(SomeTypeRep, ModifierFunction))
  +: fun   (genPair @SomeTypeRep @ModifierFunction)
  +: fun   (genPair @(NonEmpty SomeTypeRep) @Value)
  +: fun   (genList @Function)
  +: fun   (genList @SomeTypeRep)
  +: fun   (genList @(SomeTypeRep, Maybe SomeTypeRep))
  +: fun   (genList @Value)
  +: fun   (genList @Function)
  +: fun   (genMaybe @Text)
  +: fun   (genMaybe @SomeTypeRep)
  +: fun   (genPair @SomeTypeRep @(Maybe SomeTypeRep))
  +: fun   (genList @Text)
  +: fun   genInt
  +: fun   genText
  +: fun   genTextToInt
  +: fun   genDynamic
  +: fun   genSomeTypeRep
  +: end

-- * generators
newtype TextToInt = TextToInt (Text -> Int)
instance Show TextToInt where show _ = "<function>"
instance Eq TextToInt where _ == _ = True

genTextToInt :: Gen TextToInt
genTextToInt = pure (TextToInt T.length)

data UntypedRegistry = UntypedRegistry {
    _uvalues          :: Values
  , _ufunctions       :: Functions
  , _uspecializations :: Specializations
  , _umodifiers       :: Modifiers
  } deriving (Show)

genValues :: Gen (Int, Values)
genValues = do
  value  <- genInt
  values  <- makeUnsafe @(Gen Values) gensRegistry
  pure (value, createValue value `addValue` values)

genSomeTypeRep :: Gen Value -> Gen SomeTypeRep
genSomeTypeRep = fmap valueDynTypeRep

genDynamic :: Gen Dynamic
genDynamic = Gen.element [toDyn (1 :: Int), toDyn (2 :: Int), toDyn ("1" :: Text)]

genList :: forall a . Gen a -> Gen [a]
genList = Gen.list (Range.linear 1 3)

genNonEmpty :: forall a . Gen a -> Gen (NonEmpty a)
genNonEmpty genA = do
  ls <- Gen.list (Range.linear 1 3) genA
  case ls of
    -- this case can not happen
    [] -> pure <$> genA
    as -> pure (fromList as)

genMaybe :: forall a . Gen a -> Gen (Maybe a)
genMaybe = Gen.maybe

genPair :: forall a b . Gen a -> Gen b -> Gen (a, b)
genPair gena genb = (,) <$> gena <*> genb

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 5)

genText :: Gen Text
genText = Gen.text (Range.linear 2 10) Gen.alphaNum

genModifierFunction :: Gen Function -> Gen ModifierFunction
genModifierFunction genF = do
  f <- genF
  pure (const f)
