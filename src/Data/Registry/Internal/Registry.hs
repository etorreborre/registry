{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE UndecidableInstances       #-}

{-
  Internal structure of a registry and
  associated functions
-}
module Data.Registry.Internal.Registry where

import           Control.Monad.Trans.Writer.Lazy
import           Data.Dynamic
import           Data.Registry.Internal.Dynamic
import           Data.Registry.Internal.Types
import           Data.Registry.Internal.Stack
import qualified Prelude                         (show)
import           Protolude                       as P
import           Type.Reflection

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

-- | Given a newly built value, check if there are modifiers for that
--   value and apply them before "storing" the value which means
--   adding it on top of the registry, represented by the `Values` state
--   in StateT Values.
--   We use a StateT Either because applying modifiers could fail and we want
--   to catch and report the error. Note that this error would be an implementation
--   error (and not a user error) since at the type-level everything should be correct
--
--   We also use a WriterT to "log" all actions
storeValue
  :: Modifiers
  -> Dynamic
  -> Stack Dynamic
storeValue (Modifiers ms) value =
  let modifiers = findModifiers ms

  in  do valueToStore <- modifyValue value modifiers
         modify (addValue (Untyped valueToStore (show . dynTypeRep $ value)))
         pure valueToStore
  where
    -- find the applicable modifiers
    findModifiers = filter (\(m, _) -> dynTypeRep value == m)

    -- apply a list of modifiers to a value
    modifyValue v [] = pure v
    modifyValue v ((_, f) : rest) = do
      applied <- lift . lift $ applyFunction f [v]
      modifyValue applied rest
