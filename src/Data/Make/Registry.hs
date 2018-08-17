{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-

 This datatype can be used to make a Registry for
 a set of types a b c d,...

 A registry is a set of values for a list of types where
  some values are optional and some values are mandatory.

 A empty registry for a list of types will be generally obtained
 with the `registry` function

 -- the first parameter list is for optional parameters the second
 -- one for the mandatory elements
 myRegistry :: Registry '[M1, M2, M3] '[]
 myRegistry = registry

It is possible to set a value on a registry by using the override function

  myRegistry' = override (M4 1) myRegistry

A `Registry opt mand` has a Register instance for all its type paramaters
and a `Make (Registry opt mand) a` instance for each type `a` included in the mand list.

This means that a `Registry` can be used in conjonction with the `Make` typeclass
 to "make" an object like this

  makeM2 :: M2
  makeM2 = evalState make (registry :: Registry '[M1, M2] '[C1, C2])


IMPORTANT note: to simplify the implementation we do not check if
  the registry types are all distincts. This means that the same
  type could be represented twice in a Registry and when `access`-ing
  it with the Register typeclass only the first value of that type will
  be retrieved

-}
module Data.Make.Registry where

import           Data.Make.Register
import           Prelude            (Show (..), String, show, unlines)
import           Protolude          hiding (Proxy, Set, show)

-- | A Registry is either empty
--   or it contains nothing for a given type and another registry
--   or it contains a value for a given type and another registry
data Registry (optional :: [*]) (mandatory :: [*]) =
  Registry {
    _optional  :: Optional optional
  , _mandatory :: Mandatory mandatory
  }

data Optional (opt :: [*]) where
  RNil  :: Optional '[]
  RJust :: !e -> Optional opt -> Optional (e ': opt)
  RNone :: Optional opt-> Optional (e ': opt)

data Mandatory (mand :: [*]) where
  REnd  :: Mandatory '[]
  RMand :: !e -> Mandatory mand -> Mandatory (e ': mand)

-- | Create a single mandatory element
single :: a -> Registry '[] '[a]
single a = Registry RNil (RMand a REnd)

-- | Append a mandatory element to a registry
infixr 5 +:
(+:) :: e -> Registry opt mand -> Registry opt (e ': mand)
(+:) e (Registry opt mand) = Registry opt (RMand e mand)

-- | Empty registry
end :: Registry '[] '[]
end = Registry RNil REnd

-- | Show instance for mandatory elements
instance ShowAsList mand => Show (Registry opt mand) where
  show (Registry _ mand) = "[" <> unlines (showAsList mand) <> "]"

class ShowAsList (mand :: [*]) where
  showAsList :: Mandatory mand -> [String]

instance {-# OVERLAPPING #-} ShowAsList '[] where
  showAsList REnd = []

instance {-# OVERLAPPABLE #-} (Show a, ShowAsList mand) => ShowAsList (a ': mand) where
  showAsList (RMand a rest) = show a : showAsList rest

-- | Type class for getting a value out of a Registry
class GetFrom a opt mand where
  getFrom :: Registry opt mand -> Maybe a

type family If (b :: Bool) (c :: k) (d :: k) :: k where
  If 'True  c _ = c
  If 'False _ d = d

class IsBool b where
  _If
    :: forall r
    .  (('True  ~ b) => r)
    -> (('False ~ b) => r)
    -> r

instance IsBool 'True where
  _If :: forall r. r -> (('False ~ 'True) => r) -> r
  _If x _ = x

instance IsBool 'False where
  _If :: forall r. (('True ~ 'False) => r) -> r -> r
  _If _ y = y

type If' b c d = (IsBool b, If b c d)

type family IsElem a (els :: [*]) where
  IsElem a '[]         = 'False
  IsElem a (a ': _)    = 'True
  IsElem a (b ': rest) = IsElem a rest

instance If' (IsElem a opt) (GetFromOpt a opt) (ExtractFromMand a mand) => GetFrom a opt mand where
  getFrom (Registry opt mand) =
    _If @(IsElem a opt) (getFromOpt opt) (Just $ extractFromMand mand)

class GetFromOpt a opt where
  getFromOpt :: Optional opt-> Maybe a

instance {-# OVERLAPPING #-} GetFromOpt a '[] where
  getFromOpt RNil = Nothing

instance {-# OVERLAPPING #-} GetFromOpt a (a ': opt) where
  getFromOpt (RNone _)   = Nothing
  getFromOpt (RJust a _) = Just a

instance {-# OVERLAPPABLE #-} GetFromOpt a opt => GetFromOpt a (b ': opt) where
  getFromOpt (RNone rest)   = getFromOpt rest
  getFromOpt (RJust _ rest) = getFromOpt rest

-- | Typeclass for extracting a mandatory element
class ExtractFrom a opt mand where
  extractFrom :: Registry opt mand -> a

instance (ExtractFromMand a mand) => ExtractFrom a opt mand where
  extractFrom (Registry _ mand) = extractFromMand mand

class ExtractFromMand a mand where
  extractFromMand :: Mandatory mand -> a

instance {-# OVERLAPPING #-} ExtractFromMand a '[a] where
  extractFromMand (RMand a _) = a

instance {-# OVERLAPPING #-} ExtractFromMand a (a ': mand) where
  extractFromMand (RMand a _) = a

instance {-# OVERLAPPING #-} ExtractFromMand a mand => ExtractFromMand a (b ': mand) where
  extractFromMand (RMand _ rest) = extractFromMand rest


-- | Type class for setting a value in a Registry
class Override a opt mand where
  override :: a -> Registry opt mand -> Registry opt mand

instance If' (IsElem a opt) (OverrideOpt a opt) (OverrideMand a mand) => Override a opt mand where
  override a (Registry opt mand) =
    _If @(IsElem a opt) (Registry (overrideOpt a opt) mand) (Registry opt (overrideMand a mand))

class OverrideOpt a opt where
  overrideOpt :: a -> Optional opt -> Optional opt

instance {-# OVERLAPPING #-} OverrideOpt a (a ': opt) where
  overrideOpt a (RNone rest)   = RJust a rest
  overrideOpt a (RJust _ rest) = RJust a rest

instance {-# OVERLAPPABLE #-} OverrideOpt a opt => OverrideOpt a (b ': opt) where
  overrideOpt a (RNone rest)   = RNone (overrideOpt a rest)
  overrideOpt a (RJust b rest) = RJust b (overrideOpt a rest)

class OverrideMand a mand where
  overrideMand :: a -> Mandatory mand -> Mandatory mand

instance {-# OVERLAPPING #-} OverrideMand a (a ': mand) where
  overrideMand a (RMand _ rest) = RMand a rest

instance {-# OVERLAPPABLE #-} OverrideMand a mand => OverrideMand a (b ': mand) where
  overrideMand a (RMand b rest) = RMand b (overrideMand a rest)

-- | If we can get and set elements in a Registry for a given list of types
--   then we can have a Register instance
instance (GetFrom a opt mand, Override a opt mand) => Register (Registry opt mand) a where
  access   = getFrom
  register = override

registry :: (DefOptional opt) => Registry opt '[]
registry = Registry optionalDef REnd

-- | Typeclass for getting an empty Registry
class DefOptional (opt :: [*]) where
  optionalDef :: Optional opt

instance DefOptional '[] where
  optionalDef = RNil

instance DefOptional as => DefOptional (a ': as) where
  optionalDef = RNone optionalDef

-- | Extracted from the typelevel-sets project and adapted for the Registry datatype
-- | This union deduplicates elements only
--   if they appear in contiguously:
--   Registry '[A1, A2, A3, A4] `add` Registry '[A4, A3, A0] = Registry '[A1', A2, A3, A4, A0]
--   this is a lot faster than Union in terms of compilation times

type family (:++) (x :: [k]) (y :: [k]) :: [k] where
  '[]       :++ xs = xs
  (x ': xs) :++ ys = x ': (xs :++ ys)

add :: Registry s1 t1 -> Registry s2 t2 -> Registry (s1 :++ s2) (t1 :++ t2)
add (Registry opt1 mand1) (Registry opt2 mand2) = Registry (opt1 `addOpt` opt2) (mand1 `addMand` mand2)

addOpt :: Optional t1 -> Optional t2 -> Optional (t1 :++ t2)
addOpt RNil ys         = ys
addOpt (RNone xs) ys   = RNone   (addOpt xs ys)
addOpt (RJust e xs) ys = RJust e (addOpt xs ys)

addMand :: Mandatory t1 -> Mandatory t2 -> Mandatory (t1 :++ t2)
addMand REnd ys         = ys
addMand (RMand a xs) ys = RMand a (addMand xs ys)
