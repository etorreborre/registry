{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds      #-}

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

makeDot :: forall a ins out . (Typeable a, Contains a out, Solvable ins out)
  => Registry ins out
  -> Dot
makeDot = makeDotUnsafe @a

makeDotFast :: forall a ins out . (Typeable a, Contains a out)
  => Registry ins out
  -> Dot
makeDotFast = makeDotUnsafe @a

makeDotEither :: forall a ins out . (Typeable a) => Registry ins out -> Either Text Dot
makeDotEither r = toDot <$> makeOperationsEither @a r

makeDotUnsafe :: forall a ins out . (Typeable a) => Registry ins out -> Dot
makeDotUnsafe = toDot . makeOperationsUnsafe @a

makeOperationsEither :: forall a ins out . (Typeable a) => Registry ins out -> Either Text Operations
makeOperationsEither registry =
  let values          = _values registry
      functions       = _functions registry
      specializations = _specializations registry
      modifiers       = _modifiers registry
      targetType      = someTypeRep (Proxy :: Proxy a)
  in
      -- | use the makeUntyped function to create an element of the target type from a list of values and functions
      --   the list of values is kept as some State so that newly created values can be added to the current state
      case
        (flip evalStack) values
          (makeUntyped targetType (Context [targetType]) functions specializations modifiers)

      of
        Left e ->
          Left $ "could not create a " <> show targetType <> " out of the registry because " <> e <> "\nThe registry is\n" <>
                 show registry

        other ->
          other

makeOperationsUnsafe  :: forall a ins out . (Typeable a) => Registry ins out -> Operations
makeOperationsUnsafe registry =
  case makeOperationsEither @a registry of
    Right a -> a
    Left  e -> Prelude.error (toS e)
