{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds      #-}

{- |
  This modules provides functions to extract
  a DOT graph (https://en.wikipedia.org/wiki/DOT_(graph_description_language)
  out of a 'Registry'.
-}
module Data.Registry.Dot (
  module O
, makeDot
, makeDotEither
, makeDotFast
, makeDotUnsafe
, makeOperationsEither
, makeOperationsUnsafe
) where

import           Data.Registry.Internal.Make
import           Data.Registry.Internal.Operations as O
import           Data.Registry.Internal.Stack
import           Data.Registry.Internal.Types
import           Data.Registry.Registry
import           Data.Registry.Solver
import           Prelude                           (error)
import           Protolude
import           Type.Reflection

-- | Make a DOT graph for a specific value `a` built from the 'Registry'
--   `a` is at the root of the graph and its children are values
--   needed to build `a`
makeDot :: forall a ins out . (Typeable a, Contains a out, Solvable ins out)
  => Registry ins out
  -> Dot
makeDot = makeDotUnsafe @a

-- | Similar to `make` but does not check if `a` can be made out of the Regisry
--   You can use this version to get faster compilation times
makeDotFast :: forall a ins out . (Typeable a, Contains a out)
  => Registry ins out
  -> Dot
makeDotFast = makeDotUnsafe @a

-- | Similar to `make` but does not check if `a` can be made out of the 'Regisry'
--   It returns a Left value if that's not the case
makeDotEither :: forall a ins out . (Typeable a) => Registry ins out -> Either Text Dot
makeDotEither r = toDot <$> makeOperationsEither @a r

-- | Similar to `make` but does not check if `a` can be made out of the 'Regisry'
--   and throws an exception if that's not the case
makeDotUnsafe :: forall a ins out . (Typeable a) => Registry ins out -> Dot
makeDotUnsafe = toDot . makeOperationsUnsafe @a

-- | Return an `Operations` value listing all the function applications necessary to
--   create a value of a given type
makeOperationsEither :: forall a ins out . (Typeable a) => Registry ins out -> Either Text Operations
makeOperationsEither registry =
  let values          = _values registry
      functions       = _functions registry
      specializations = _specializations registry
      modifiers       = _modifiers registry
      targetType      = someTypeRep (Proxy :: Proxy a)
  in
      -- use the makeUntyped function to create an element of the target type from a list of values and functions
      -- the list of values is kept as some State so that newly created values can be added to the current state
      case
        (flip evalStack) values
          (makeUntyped targetType (Context [targetType]) functions specializations modifiers)

      of
        Left e ->
          Left $ "could not create a " <> show targetType <> " out of the registry because " <> e <> "\nThe registry is\n" <>
                 show registry

        other ->
          other

-- | Return an `Operations` value listing all the function applications necessary to
--   create a value of a given type (and throws an exception if the value cannot be created)
makeOperationsUnsafe  :: forall a ins out . (Typeable a) => Registry ins out -> Operations
makeOperationsUnsafe registry =
  case makeOperationsEither @a registry of
    Right a -> a
    Left  e -> Prelude.error (toS e)
