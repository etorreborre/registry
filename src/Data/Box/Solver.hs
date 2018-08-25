{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Data.Box.Solver where

import Data.Type.Bool
import Data.Type.Equality

import Protolude

type family Inputs f :: [*] where
  Inputs (i -> o) = i ': Inputs o
  Inputs x = '[]

type family Output f :: * where
  Output (i -> o) = Output o
  Output x = x

class Contains (a :: *) (els :: [*])
instance {-# OVERLAPPING #-} Contains a (a ': els)
instance {-# OVERLAPPABLE #-} Contains a els => Contains a (b ': els)

class Solvable (ins :: [*]) (out :: [*])
instance (IsSubset ins out) => Solvable ins out

class IsSubset (ins :: [*]) (out :: [*])
instance IsSubset '[] out
instance (Contains a out, IsSubset els out) => IsSubset (a ': els) out

-- | Extracted from the typelevel-sets project and adapted for the Registry datatype
-- | This union deduplicates elements only
--   if they appear in contiguously:
type family (:++) (x :: [k]) (y :: [k]) :: [k] where
  '[]       :++ xs = xs
  (x ': xs) :++ ys = x ': (xs :++ ys)

{-| Remove duplicates from a sorted list -}
type family Nub t where
  Nub '[]           = '[]
  Nub '[e]          = '[e]
  Nub (e ': e ': s) = Nub (e ': s)
  Nub (e ': f ': s) = e ': Nub (f ': s)

type Union s t = Nub (Sort (s :++ t))

type family Sort (xs :: [k]) :: [k] where
  Sort '[]       = '[]
  Sort (x ': xs) = ((Sort (Filter 'FMin x xs)) :++ '[x]) :++ (Sort (Filter 'FMax x xs))

data Flag = FMin | FMax

type family Filter (f :: Flag) (p :: k) (xs :: [k]) :: [k] where
  Filter f p '[]       = '[]
  Filter 'FMin p (x ': xs) = If (Cmp x p == 'LT) (x ': (Filter 'FMin p xs)) (Filter 'FMin p xs)
  Filter 'FMax p (x ': xs) = If (Cmp x p == 'GT || Cmp x p == 'EQ) (x ': (Filter 'FMax p xs)) (Filter 'FMax p xs)

-- | I need to find a way to sort types according to their names
type family Cmp (a :: k) (b :: k) :: Ordering
type instance Cmp a b = 'LT


