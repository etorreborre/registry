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

registry =
     funTo @Gen UntypedRegistry
  +: funTo @Gen Values
  +: funTo @Gen Functions
  +: funTo @Gen Specializations
  +: funTo @Gen Modifiers
  +: funTo @Gen Context
  +: funTo @Gen Untyped
  +: fun   (genList @(SomeTypeRep, Dynamic))
  +: fun   (genPair @SomeTypeRep @Dynamic)
  +: fun   (genList @Untyped)
  +: fun   (genList @SomeTypeRep)
  +: fun   genInt
  +: fun   genText
  +: fun   genFunction
  +: fun   genDynamic
  +: fun   genSomeTypeRep
  +: end

-- * generators
newtype Function = Function (Text -> Int)
instance Show Function where show _ = "<function>"
instance Eq Function where _ == _ = True

genFunction :: Gen Function
genFunction = pure (Function T.length)

data UntypedRegistry = UntypedRegistry {
    _uvalues          :: Values
  , _ufunctions       :: Functions
  , _uspecializations :: Specializations
  , _umodifiers       :: Modifiers
  } deriving (Show)

genValues :: Gen (Int, Values)
genValues = do
  value  <- gen @Int
  values <- (val value `addTypedValue`) <$> gen @Values
  pure (value, values)

genSomeTypeRep :: Gen Untyped -> Gen SomeTypeRep
genSomeTypeRep genValue = do
  Untyped a _ <- genValue
  pure $ dynTypeRep a

genDynamic :: Gen Dynamic
genDynamic = Gen.element [toDyn (1 :: Int), toDyn (2 :: Int), toDyn ("1" :: Text)]

forall :: forall a . (Typeable a, Show a) => PropertyT IO a
forall = forAll $ makeUnsafe @(Gen a) registry

genList :: forall a . Gen a -> Gen [a]
genList = Gen.list (Range.linear 1 3)

genPair :: forall a b . Gen a -> Gen b -> Gen (a, b)
genPair gena genb = (,) <$> gena <*> genb

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 5)

gen :: forall a . (Typeable a) => Gen a
gen = makeUnsafe registry

genText :: Gen Text
genText = Gen.text (Range.linear 2 10) Gen.alphaNum
