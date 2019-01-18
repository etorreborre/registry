{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
  Internal structure of a 'Registry' and
  associated functions
-}
module Data.Registry.Internal.Registry where

import           Data.List                      (dropWhileEnd, elemIndex)
import           Data.List.NonEmpty             as NonEmpty (last)
import           Data.Registry.Internal.Dynamic
import           Data.Registry.Internal.Stack
import           Data.Registry.Internal.Types
import           Protolude                      as P
import           Type.Reflection

-- | Find a value having a target type from:
--     - a list of "preferred values" (Specializations) to select when we are trying
--        to find the targe in a specific context (Context). Context describes
--       the types of values we are currently trying to (recursively) make
--
--     - a list of dynamic values (Values)
--
--  2 subtleties:
--    1. if there are specialized values we need to find the most specialized for
--      the current context, that is the one having its "targetType" the "lowest" in the
--      values graph
--
--    2. if an already created value have the right type but if it is a specialization
--       and the type we are looking for is notin the specialization context
--       then we cannot use that value, we need to recreate a brand new one
--
findValue ::
     SomeTypeRep
  -> Context
  -> Specializations
  -> Values
  -> Maybe Value
findValue target (Context cs) (Specializations sp) (Values vs) =
  -- try to find the target value in the list of specializations first
  let
      -- a specialization can only be used if its context types are part of the current context
      specializationCandidates = filter (\(ts, v) -> target == valueDynTypeRep v && all (`elem` cs) ts) sp
      -- the best specialization is the one having its last context type the deepest in the current context
      bestSpecialization = sortOn (flip elemIndex cs . NonEmpty.last . fst) specializationCandidates

      bestSpecializedValue = head $ asCreatedValue <$> bestSpecialization
      asCreatedValue (ts, ProvidedValue d desc) = CreatedValue d desc (Just $ Context (dropWhileEnd (/= last ts) cs))
      asCreatedValue (_, v)                     = v

      targetValue = head $ filter (\v -> valueDynTypeRep v == target && not (isInSpecializationContext target v)) vs

  in  bestSpecializedValue <|>
      targetValue

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
