{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Registry.Internal.Types where

import           Data.Dynamic
import           Prelude         (show)
import           Protolude       hiding (show)
import           Type.Reflection

-- List of types currently being built
newtype Context = Context { _context :: [SomeTypeRep] } deriving (Show, Semigroup, Monoid)

-- List of functions available for constructing other values
newtype Functions = Functions [Untyped] deriving (Show, Semigroup, Monoid)

addTypedFunction :: Typed a -> Functions -> Functions
addTypedFunction = addFunction . toUntyped

addFunction :: Untyped -> Functions -> Functions
addFunction f (Functions fs) = Functions (f : fs)

-- List of values available for constructing other values
newtype Values = Values [Untyped] deriving (Show, Semigroup, Monoid)

addTypedValue :: Typed a -> Values -> Values
addTypedValue = addValue . toUntyped

addValue :: Untyped -> Values -> Values
addValue v (Values vs) = Values (v : vs)

data Typed a = Typed Dynamic Text

toUntyped :: Typed a -> Untyped
toUntyped (Typed a t) = Untyped a t

dynTypeRepOf :: Typed a -> SomeTypeRep
dynTypeRepOf (Typed a _) = dynTypeRep a

valueOf :: Typed a -> Dynamic
valueOf (Typed a _) = a

instance Show (Typed a) where
  show (Typed _ t) = toS t

data Untyped = Untyped {
  _value       :: Dynamic
, _description :: Text
}  deriving (Show)

-- Specification of values which become available for
-- construction when a corresponding type comes in context
newtype Specializations = Specializations [(SomeTypeRep, Dynamic)] deriving (Show, Semigroup, Monoid)

-- List of functions modifying some values right after they have been
-- built. This enables "tweaking" the creation process with slightly
-- different results. Here SomeTypeRep is the target value type 'a' and
-- Dynamic is an untyped function a -> a
newtype Modifiers = Modifiers [(SomeTypeRep, Dynamic)] deriving (Show, Semigroup, Monoid)
