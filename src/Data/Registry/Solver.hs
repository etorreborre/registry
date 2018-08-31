{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

{-
  Type level functions to statically assess
  if a value can be built out of a Registry

  For now we don't check if there could be cycles in
    the registry functions
-}
module Data.Registry.Solver where

-- | Compute the list of input types for a function
type family Inputs f :: [*] where
  Inputs (i -> o) = i ': Inputs o
  Inputs x = '[]

-- | Compute the output type for a function
type family Output f :: * where
  Output (i -> o) = Output o
  Output x = x

-- | Compute if a type is contained in a list of types
class Contains (a :: *) (els :: [*])
instance {-# OVERLAPPING #-} Contains a (a ': els)
instance {-# OVERLAPPABLE #-} Contains a els => Contains a (b ': els)

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
