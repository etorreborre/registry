{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-
  This code is taken from https://stackoverflow.com/questions/28003135/is-it-possible-to-encode-a-generic-lift-function-in-haskell
  to allow a generic lift operation over an Applicative context
  So if you have a function: Int -> Text -> IO Int, it can be lifted to have all of its parameters
    in IO:

    f :: Int -> Text -> IO Int

    lifted :: IO Int -> IO Text -> IO Int
    lifted = to @IO f

-}
module Data.Registry.Lift where

import           Protolude

-- | Typeclass for lifting pure functions to effectful arguments and results
class Applicative f => ApplyVariadic f a b where
  applyVariadic :: f a -> b

instance (Applicative f, b ~ f a) => ApplyVariadic f a b where
  applyVariadic = identity

instance (Applicative f, ApplyVariadic f a' b', b ~ (f a -> b')) => ApplyVariadic f (a -> a') b where
  applyVariadic f fa = applyVariadic (f <*> fa)

-- | Lift a pure function to effectful arguments and results
allTo :: forall f a b. ApplyVariadic f a b => a -> b
allTo a = (applyVariadic :: f a -> b) (pure a)

-- | Typeclass for lifting impure functions to effectful arguments and results
class Monad f => ApplyVariadic1 f a b where
  applyVariadic1 :: f a -> b

instance (Monad f, b ~ f a) => ApplyVariadic1 f (f a) b where
  applyVariadic1 = join

instance (Monad f, ApplyVariadic1 f a' b', b ~ (f a -> b')) => ApplyVariadic1 f (a -> a') b where
  applyVariadic1 f fa = applyVariadic1 (f <*> fa)

-- | Lift an effectful function to effectful arguments and results
argsTo :: forall f a b . ApplyVariadic1 f a b => a -> b
argsTo a = (applyVariadic1 :: f a -> b) (pure a)

-- | Typeclass for lifting a function with a result of type m b into a function
--   with a result of type n b
class Applicative f => ApplyVariadic2 f g a b where
  applyVariadic2 :: (forall x . f x -> g x) -> a -> b

instance (Applicative f, b ~ g a) => ApplyVariadic2 f g (f a) b where
  applyVariadic2 natfg = natfg

instance (Applicative f, ApplyVariadic2 f g a' b', b ~ (a -> b')) => ApplyVariadic2 f g (a -> a') b where
  applyVariadic2 natfg f a = applyVariadic2 natfg (f a)

-- | Lift a function returning an effectful result to a function returning another effectful result
outTo :: forall g f a b . ApplyVariadic2 f g a b => (forall x . f x -> g x) -> a -> b
outTo natfg = applyVariadic2 natfg :: a -> b
