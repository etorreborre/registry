{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}

{-
  This code is taken from https://stackoverflow.com/questions/28003135/is-it-possible-to-encode-a-generic-lift-function-in-haskell
  to allow a generic lift operation over an Applicative context
  So if you have a function: Int -> Text -> IO Int, it can be lifted to have all of its parameters
    in IO:

    f :: Int -> Text -> IO Int

    lifted :: IO Int -> IO Text -> IO Int
    lifted = into @IO f

-}
module Data.Registry.Lift where

import        Protolude

-- | Typeclass for lifting pure functions to effectful arguments and results
class Applicative f => ApplyVariadic f a b where
  applyVariadic :: f a -> b

instance (Applicative f, b ~ f a) => ApplyVariadic f a b where
  applyVariadic = identity

instance (Applicative f, ApplyVariadic f a' b', b ~ (f a -> b')) => ApplyVariadic f (a -> a') b where
  applyVariadic f fa = applyVariadic (f <*> fa)

-- | Lift a pure function to effectful arguments and results
into :: forall f a b. ApplyVariadic f a b => a -> b
into a = (applyVariadic :: f a -> b) (pure a)

-- | Typeclass for lifting impure functions to effectful arguments and results
class Monad f => ApplyVariadic1 f a b where
  applyVariadic1 :: f a -> b

instance (Monad f, b ~ f a) => ApplyVariadic1 f (f a) b where
  applyVariadic1 = join

instance (Monad f, ApplyVariadic1 f a' b', b ~ (f a -> b')) => ApplyVariadic1 f (a -> a') b where
  applyVariadic1 f fa = applyVariadic1 (f <*> fa)

-- | Lift an effectful function to effectful arguments and results
intoM :: forall f a b . ApplyVariadic1 f a b => a -> b
intoM a = (applyVariadic1 :: f a -> b) (pure a)
