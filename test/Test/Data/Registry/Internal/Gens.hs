{-# OPTIONS_GHC -fno-warn-missing-monadfail-instances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Internal.Gens where

import           Data.Dynamic
import           Data.Registry
import           Data.Registry.Internal.Types
import           Data.Text                         as T
import           Hedgehog
import           Hedgehog.Gen                      as Gen
import           Hedgehog.Range                    as Range
import           Prelude                           (show)
import           Protolude
import           Type.Reflection

-- Hedgehog generators for the internal types
registry =
     funTo @Gen UntypedRegistry
  +: funTo @Gen Values
  +: funTo @Gen Functions
  +: funTo @Gen Specializations
  +: funTo @Gen Modifiers
  +: funTo @Gen Context
  +: funTo @Gen Function
  +: funTo @Gen ProvidedValue
  +: funTo @Gen ValueDescription
  +: funTo @Gen FunctionDescription
  +: fun   (genList @(SomeTypeRep, Function))
  +: fun   (genList @(SomeTypeRep, Value))
  +: fun   (genPair @SomeTypeRep @Function)
  +: fun   (genPair @SomeTypeRep @Value)
  +: fun   (genList @Function)
  +: fun   (genList @SomeTypeRep)
  +: fun   (genList @Value)
  +: fun   (genList @Function)
  +: fun   (genMaybe @Text)
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
  value  <- gen @Int
  values <- (createValue value `addValue`) <$> gen @Values
  pure (value, values)

genSomeTypeRep :: Gen Value -> Gen SomeTypeRep
genSomeTypeRep = fmap valueDynTypeRep

genDynamic :: Gen Dynamic
genDynamic = Gen.element [toDyn (1 :: Int), toDyn (2 :: Int), toDyn ("1" :: Text)]

forall :: forall a . (Typeable a, Show a) => PropertyT IO a
forall = forAll $ makeUnsafe @(Gen a) registry

genList :: forall a . Gen a -> Gen [a]
genList = Gen.list (Range.linear 1 3)

genMaybe :: forall a . Gen a -> Gen (Maybe a)
genMaybe = Gen.maybe

genPair :: forall a b . Gen a -> Gen b -> Gen (a, b)
genPair gena genb = (,) <$> gena <*> genb

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 5)

gen :: forall a . (Typeable a) => Gen a
gen = makeUnsafe registry

genText :: Gen Text
genText = Gen.text (Range.linear 2 10) Gen.alphaNum
