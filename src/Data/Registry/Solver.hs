{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{- |
  Type level functions to statically assess
  if a value can be built out of a Registry

  For now we don't check if there could be cycles in
    the registry functions
-}
module Data.Registry.Solver where

import           Data.Kind
import           GHC.TypeLits

-- | Compute the list of input types for a function
type family Inputs f :: [Type] where
  Inputs (i -> o) = i ': Inputs o
  Inputs x = '[]

-- | Compute the output type for a function
type family Output f :: Type where
  Output (i -> o) = Output o
  Output x = x

-- | Compute if a type is contained in a list of types
type family Contains (a :: Type) (els :: [Type]) :: Constraint where
  Contains a '[] = TypeError (Text "No element of type " ':<>: 'ShowType a ':<>: 'Text " can be built out of the registry")
  Contains a (a ': els) = ()
  Contains a (b ': els) = Contains a els

-- | Shorthand type alias when many such constraints need to be added to a type signature
type (out :- a) = Contains a out

-- | Compute if each element of a list of types is contained in
-- another list
class IsSubset (ins :: [Type]) (out :: [Type])
instance IsSubset '[] out
instance (Contains a out, IsSubset els out) => IsSubset (a ': els) out

-- | From the list of all the input types and outputs types of a registry
--   Can we create all the output types?
class Solvable (ins :: [Type]) (out :: [Type])
instance (IsSubset ins out) => Solvable ins out


-- | Extracted from the typelevel-sets project and adapted for the Registry datatype
--   This union deduplicates elements only if they appear in contiguously
--   What we really want is typelevel sets but they are too slow for now
--   https://github.com/dorchard/type-level-sets/issues/17
type family (:++) (x :: [k]) (y :: [k]) :: [k] where
  '[]       :++ xs = xs
  (x ': xs) :++ ys = x ': (xs :++ ys)

-- | Return '[a] only if it is not already in the list of types
type family FindUnique (a :: Type) (as :: [Type]) :: [Type] where
  FindUnique a '[] = '[a]
  FindUnique a (a ': rest) = '[]
  FindUnique a (b ': rest) = FindUnique a rest

type family Normalized (as :: [Type]) :: [Type] where
  Normalized '[] = '[]
  Normalized '[a] = '[a]
  Normalized (a ': rest) = FindUnique a rest :++ Normalized rest
