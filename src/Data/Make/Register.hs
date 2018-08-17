{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-
  This module defines a typeclass for boxes registration
-}
module Data.Make.Register where

import           Protolude

-- | Typeclass for registering an object in a given state object s
class Register s a where
  -- Return a value from the registry if present
  access :: s -> Maybe a

  -- Add a value to the registry
  register :: a -> s -> s

-- | "canonical" instance to hold just one value
--   this can be useful for tests
instance {-# OVERLAPPABLE #-} Register (Maybe m) m where
  access s = s
  register m = const (Just m)

-- | default instance always returning Nothing
instance Register () a where
  access = const Nothing
  register _ = identity
