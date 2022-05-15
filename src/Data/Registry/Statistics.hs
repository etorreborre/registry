{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module Data.Registry.Statistics
  ( module S,
    makeStatistics,
    makeStatisticsEither,
  )
where

import Data.Registry.Internal.Make
import Data.Registry.Internal.Stack
import Data.Registry.Internal.Statistics as S
import Data.Registry.Internal.Types
import Data.Registry.Registry
import Protolude
import Type.Reflection
import Prelude (error)

-- | Return `Statistics` as the result of the creation of a value
--   of a given type (and throws an exception if the value cannot be created)
makeStatistics :: forall a ins out. (Typeable a) => Registry ins out -> Statistics
makeStatistics registry =
  case makeStatisticsEither @a registry of
    Right a -> a
    Left e -> Prelude.error (toS e)

-- | Return `Statistics` as the result of the creation of a value
--   of a given type
makeStatisticsEither :: forall a ins out. (Typeable a) => Registry ins out -> Either Text Statistics
makeStatisticsEither registry =
  let values = _values registry
      functions = _functions registry
      specializations = _specializations registry
      modifiers = _modifiers registry
      targetType = someTypeRep (Proxy :: Proxy a)
   in -- use the makeUntyped function to create an element of the target type from a list of values and functions
      -- the list of values is kept as some State so that newly created values can be added to the current state
      case evalStackWithValues
        values
        (makeUntyped targetType (Context [(targetType, Nothing)]) functions specializations modifiers) of
        Left e ->
          Left $
            "could not create a " <> show targetType <> " out of the registry because " <> e <> "\nThe registry is\n"
              <> show registry
        other ->
          other
