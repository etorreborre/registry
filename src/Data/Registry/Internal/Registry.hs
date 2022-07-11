{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--  Internal structure of a Registry and
--  associated functions
module Data.Registry.Internal.Registry where

import Data.Registry.Internal.Dynamic
import Data.Registry.Internal.Stack
import Data.Registry.Internal.Types
import Protolude as P
import Type.Reflection

-- | Find a value having a target type from:
--     - a list of "preferred values" (Specializations) to select when we are trying
--        to find the targe in a specific context (Context). Context describes
--       the types of values we are currently trying to (recursively) make
--
--     - a list of already created values (Values)
--
--  3 subtleties:
--    1. if there are specialized values we need to find the most specialized for
--      the current context, that is the one having its "targetType" the "lowest" in the
--      values graph
--
--    2. if an already created value has the right type but if it is a specialization
--       and the type we are looking for is not in the specialization context
--       then we cannot use that value, we need to recreate a brand new one
--
--    3. if an already created value has the right type and is not specialized
--       but if there is an incompatible specialization for one of its dependencies
--       then it cannot be used
findValue ::
  SomeTypeRep ->
  Context ->
  Specializations ->
  Values ->
  Maybe Value
findValue target context specializations values =
  let -- 1. first try to find the target value in the list of specializations
      -- those all are all the specializations which make sense in this context
      applicableSpecializations = (specializations `applicableTo` context)
      bestSpecializedValue = findBestSpecializedValue target context applicableSpecializations

      compatibleValue = findCompatibleCreatedValue target specializations values
   in bestSpecializedValue <|> compatibleValue

-- | Among all the applicable specializations take the most specific one
--   if there exists any
findBestSpecializedValue :: SomeTypeRep -> Context -> Specializations -> Maybe Value
findBestSpecializedValue target context (Specializations sp) =
  let -- the candidates must have the required type
      specializationCandidates = filter (\s -> target == specializationTargetType s) sp
      -- the best specialization is the one having its last context type the deepest in the current context
      bestSpecializations = sortOn (specializedContext context) specializationCandidates
      bestSpecializedValue = head bestSpecializations
   in createValueFromSpecialization context <$> bestSpecializedValue

-- | Among all the created values, take a compatible one
--
--    - if that value is a specialized value or has specialized
--      dependencies it must be compatible with the current context
findCompatibleCreatedValue :: SomeTypeRep -> Specializations -> Values -> Maybe Value
findCompatibleCreatedValue target specializations (Values vs) =
  let isApplicableValue value = valueDynTypeRep value == target
      isNotSpecializedForAnotherContext value =
        not (hasSpecializedDependencies specializations value)
          && not (isInSpecializationContext target value)

      applicableValues = filter ((&&) <$> isApplicableValue <*> isNotSpecializedForAnotherContext) vs
   in head applicableValues

-- | Find a constructor function returning a target type
--   from a list of constructors
findConstructor ::
  SomeTypeRep ->
  Functions ->
  Maybe Function
findConstructor _ (Functions []) = Nothing
findConstructor target (Functions (f : rest)) =
  case funDynTypeRep f of
    SomeTypeRep (Fun _ out) ->
      if outputType (SomeTypeRep out) == target
        then Just f
        else findConstructor target (Functions rest)
    -- a "function" with no arguments
    SomeTypeRep out ->
      if outputType (SomeTypeRep out) == target
        then Just f
        else findConstructor target (Functions rest)

-- | Given a newly built value, check if there are modifiers for that
--   value and apply them before "storing" the value which means
--   adding it on top of the registry, represented by the `Values` state
--   in StateT Values.
--   We use a StateT Either because applying modifiers could fail and we want
--   to catch and report the error. Note that this error would be an implementation
--   error (and not a user error) since at the type-level everything should be correct
storeValue ::
  Modifiers ->
  Value ->
  Stack Value
storeValue (Modifiers ms) value =
  let modifiers = findModifiers ms
   in do
        valueToStore <- modifyValue value modifiers
        modifyValues (addValue valueToStore)
        pure valueToStore
  where
    -- find the applicable modifiers
    findModifiers = filter (\(m, _) -> valueDynTypeRep value == m)

    -- apply a list of modifiers to a value
    modifyValue :: Value -> [(SomeTypeRep, ModifierFunction)] -> Stack Value
    modifyValue v [] = pure v
    modifyValue v ((_, f) : rest) = do
      applied <- lift $ applyModification (f (specializationPaths v)) v
      modifyValue applied rest
