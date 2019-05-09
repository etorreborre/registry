{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Registry.State where

import           Control.Monad.Morph
import           Data.Registry.Internal.Types
import           Data.Registry.Lift
import           Data.Registry.Registry
import           Data.Registry.Solver
import           Protolude

-- | Run some registry modifications in the StateT monad
runS :: (MFunctor m, Monad n) => Registry ins out -> m (StateT (Registry ins out) n) a -> m n a
runS r = hoist (`evalStateT` r)

-- | Add an element to the registry without changing its type
addFunTo :: forall m a b ins out . (ApplyVariadic m a b, Typeable a, Typeable b, IsSubset (Inputs b) ins, Contains (Output b) out) => a -> Registry ins out -> Registry ins out
addFunTo = modifyRegistry @b . funTo @m

-- | Add an element to the registry without changing its type, in the State monad
addFunS :: (Typeable a, IsSubset (Inputs a) ins, Contains (Output a) out, MonadState (Registry ins out) m) => a -> m ()
addFunS = modify . addFun

-- | Add an element to the registry without changing its type, in the State monad
addToS :: forall n a b m ins out . (ApplyVariadic n a b, Typeable a, Typeable b, Typeable a, IsSubset (Inputs b) ins, Contains (Output b) out, MonadState (Registry ins out) m) => a -> m ()
addToS = modify . addFunTo @n @a @b

-- | Add an element to the registry without changing its type
addFun :: (Typeable a, IsSubset (Inputs a) ins, Contains (Output a) out) => a -> Registry ins out -> Registry ins out
addFun = modifyRegistry . fun

-- | Register modifications of elements which types are already in the registry
modifyRegistry :: (Typeable a, IsSubset (Inputs a) ins, Contains (Output a) out) => Typed a -> Registry ins out -> Registry ins out
modifyRegistry (TypedValue v) (Registry (Values vs) functions specializations modifiers) =
  Registry (Values (v : vs)) functions specializations modifiers

modifyRegistry (TypedFunction f) (Registry (Values vs) (Functions fs) specializations modifiers) =
  Registry (Values vs) (Functions (f : fs)) specializations modifiers
