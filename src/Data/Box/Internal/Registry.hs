{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Box.Internal.Registry where

import           Data.Box.Internal.Dynamic
import           Data.Dynamic
import qualified Prelude                 (show)
import           Protolude               as P
import           Type.Reflection

-- List of types currently being built
newtype Context = Context [SomeTypeRep] deriving (Show)

-- List of functions available for constructing other values
newtype Functions = Functions [Untyped]

-- List of values available for constructing other values
newtype Values = Values [Untyped]

addValue :: Untyped -> Values -> Values
addValue v (Values vs) = Values (v : vs)

data Typed a = Typed Dynamic Text

instance Show (Typed a) where
  show (Typed _ t) = toS t

data Untyped = Untyped {
  _value       :: Dynamic
, _description :: Text
}

-- Specification of values which become available for
-- construction when a corresponding type comes in context
newtype Specializations = Specializations [(SomeTypeRep, Dynamic)] deriving (Show)

-- List of functions modifying some values right after they have been
-- built. This enables "tweaking" the creation process with slightly
-- different results. Here SomeTypeRep is the target value type 'a' and
-- Dynamic is an untyped function a -> a
newtype Modifiers = Modifiers [(SomeTypeRep, Dynamic)] deriving (Show)

storeValue :: Modifiers -> Dynamic -> State Values Dynamic
storeValue (Modifiers ms) value =
  let valueToStore = case findModifier ms of
        Nothing     -> value
        Just (_, f) -> applyFunction f [value]
  in  modify (addValue (Untyped valueToStore (show . dynTypeRep $ value))) >>
      pure valueToStore
  where findModifier = find (\(m, _) -> dynTypeRep value == m)

-- | Find a value having a target type
--   from a list of dynamic values found in a list of constructors
--   where some of them are not functions
--   There is also a list of specializations when we can specialize the values to use
--   if a given type is part of the context
findValue
  :: SomeTypeRep
  -> Context
  -> Specializations
  -> Values
  -> Maybe Dynamic
-- no specializations or constructors to choose from
findValue _ _ (Specializations []) (Values []) = Nothing

-- recurse on the specializations first
findValue target (Context context) (Specializations ((t, v) : rest)) values =
  -- if there is an override which value matches the current target
  -- and if that override is in the current context then return the value
  if target == dynTypeRep v && t `elem` context then
    Just v
  else
    findValue target (Context context) (Specializations rest) values

-- otherwise recurse on the list of constructors until a value
-- with the target type is found
findValue target context specializations (Values (Untyped t _ : rest)) =
  if dynTypeRep t == target then
    Just t
  else
    findValue target context specializations (Values rest)

-- | Find a constructor function returning a target type
--   from a list of constructorsfe
findConstructor
  :: SomeTypeRep
  -> Functions
  -> Maybe Dynamic

findConstructor _      (Functions []        ) = Nothing
findConstructor target (Functions (Untyped c _ : rest)) =
  case dynTypeRep c of
    SomeTypeRep (Fun _ out) ->
      if outputType (SomeTypeRep out) == target then
        Just c
      else
        findConstructor target (Functions rest)

    _ -> findConstructor target (Functions rest)
