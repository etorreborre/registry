{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE UndecidableInstances       #-}

{- |
  Internal structure of a 'Registry' and
  associated functions
-}
module Data.Registry.Internal.Registry where

import           Data.Registry.Internal.Dynamic
import           Data.Registry.Internal.Types
import           Data.Registry.Internal.Stack
import           Protolude                       as P
import           Type.Reflection

-- | Find a value having a target type from:
--     - a list of "preferred values" (Specializations) to select when we are trying
--        to find the targe in a specific context (Context). Context describes
--       the types of values we are currently trying to (recursively) make
--
--     - a list of dynamic values (Values)
findValue ::
     SomeTypeRep
  -> Context
  -> Specializations
  -> Values
  -> Maybe Value
-- no specializations or values to choose from
findValue _ _ (Specializations []) (Values []) = Nothing

-- recurse on the specializations first
findValue target (Context context) (Specializations ((t, v) : rest)) values =
  -- if there is an override which value matches the current target
  -- and if that override is in the current context then return the value
  if target == valueDynTypeRep v && t `elem` context then
    Just v
  else
    findValue target (Context context) (Specializations rest) values

-- otherwise recurse on the list of constructors until a value
-- with the target type is found
findValue target context specializations (Values (v : rest)) =
  if valueDynTypeRep v == target then
    Just v
  else
    findValue target context specializations (Values rest)

-- | Find a constructor function returning a target type
--   from a list of constructors
findConstructor ::
     SomeTypeRep
  -> Functions
  -> Maybe Function
findConstructor _      (Functions []        ) = Nothing
findConstructor target (Functions (f : rest)) =
  case funDynTypeRep f of
    SomeTypeRep (Fun _ out) ->
      if outputType (SomeTypeRep out) == target then
        Just f
      else
        findConstructor target (Functions rest)

    -- a "function" with no arguments
    SomeTypeRep out ->
      if outputType (SomeTypeRep out) == target then
        Just f
     else
        findConstructor target (Functions rest)

-- | Given a newly built value, check if there are modifiers for that
--   value and apply them before "storing" the value which means
--   adding it on top of the registry, represented by the `Values` state
--   in StateT Values.
--   We use a StateT Either because applying modifiers could fail and we want
--   to catch and report the error. Note that this error would be an implementation
--   error (and not a user error) since at the type-level everything should be correct
--
storeValue ::
     Modifiers
  -> Value
  -> Stack Value
storeValue (Modifiers ms) value =
  let modifiers = findModifiers ms

  in  do valueToStore <- modifyValue value modifiers
         modifyValues (addValue valueToStore)
         pure valueToStore
  where
    -- find the applicable modifiers
    findModifiers = filter (\(m, _) -> valueDynTypeRep value == m)

    -- apply a list of modifiers to a value
    modifyValue :: Value -> [(SomeTypeRep, Function)] -> Stack Value
    modifyValue v [] = pure v
    modifyValue v ((_, f) : rest) = do
      applied <- lift $ applyFunction f [v]
      modifyValue applied rest
