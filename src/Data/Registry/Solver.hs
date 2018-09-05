{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-
  Type level functions to statically assess
  if a value can be built out of a Registry

  For now we don't check if there could be cycles in
    the registry functions
-}
module Data.Registry.Solver where

import           Data.Kind
import           GHC.TypeLits

-- | Compute the list of input types for a function
type family Inputs f :: [*] where
  Inputs (i -> o) = i ': Inputs o
  Inputs x = '[]

-- | Compute the output type for a function
type family Output f :: * where
  Output (i -> o) = Output o
  Output x = x

-- | Compute if a type is contained in a list of types
type family Contains (a :: *) (els :: [*]) :: Constraint where
  Contains a '[] = TypeError ('Text "No element of type " ':<>: 'ShowType a ':<>: 'Text " can be build out of the registry")
  Contains a (a ': els) = ()
  Contains a (b ': els) = Contains a els

-- | Shorthand type alias when many such constraints need to be added to a type signature
type (out :- a) = Contains a out

-- | Compute if each element of a list of types is contained in
-- another list
class IsSubset (ins :: [*]) (out :: [*])
instance IsSubset '[] out
instance (Contains a out, IsSubset els out) => IsSubset (a ': els) out

-- | From the list of all the input types and outputs types of a registry
--   Can we create all the output types?
class Solvable (ins :: [*]) (out :: [*])
instance (IsSubset ins out) => Solvable ins out


-- | Extracted from the typelevel-sets project and adapted for the Registry datatype
-- | This union deduplicates elements only
--   if they appear in contiguously:
type family (:++) (x :: [k]) (y :: [k]) :: [k] where
  '[]       :++ xs = xs
  (x ': xs) :++ ys = x ': (xs :++ ys)
