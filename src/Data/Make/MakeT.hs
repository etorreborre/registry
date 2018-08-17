{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

{-
  This module helps with the creation of dependent boxes
  where the creation can have effects
-}
module Data.Make.MakeT (
    module Register
  , MakeT(..)
  , makeInstanceT
  , storeT
  , create0
  , create1
  , create2
  , create3
  , create4
  , create5
  , create6
  , create7
  , create8
  , create9
  , make0
  , make1
  , make2
  , spawn1
  , spawn2
) where

import           Data.Make.Register as Register
import           Protolude
import           Universum          ((...))

-- | Typeclass making and storing an object
class MakeT s t a where
  makeT :: StateT s t a

-- | Make a given instance with a specific state
makeInstanceT :: forall s t a . (Monad t, MakeT s t a) => s -> t a
makeInstanceT = evalStateT makeT

-- | Register a given object and return it
storeT :: (Monad t, Register s a) => a -> StateT s t a
storeT g = modify (register g) >> pure g

-- | Generator
make0 :: (Monad t, Register s a) => a -> StateT s t a
make0 constructor = do
  s <- get
  case access s of
    Just m  -> pure m
    Nothing -> storeT constructor

make1 :: ( Functor m
         , Monad t
         , Register s (m b)
         , MakeT s t (m a)
         )
        => (m a -> m b)
        -> StateT s t (m b)
make1 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> storeT =<< constructor <$> makeT

spawn1 :: ( Functor m
           , Monad t
           , Register s (m b)
           , MakeT s t (m a)
           )
        => (a -> b)
        -> StateT s t (m b)
spawn1 constructor = make1 (constructor <$>)

make2 :: ( Functor m
         , Monad t
         , Register s (m b)
         , MakeT s t (m a1)
         , MakeT s t (m a2)
         )
        => (m a1 -> m a2 -> m b)
        -> StateT s t (m b)
make2 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> storeT =<< constructor <$> makeT <*> makeT

spawn2 :: (  Applicative m
           , Monad t
           , Register s (m b)
           , MakeT s t (m a1)
           , MakeT s t (m a2)
           )
        => (a1 -> a2 -> b)
        -> StateT s t (m b)
spawn2 constructor = make2 (\ma1 ma2 -> constructor <$> ma1 <*> ma2)

create0 :: ( Monad t
           , Register s a
           )
        => t a
        -> StateT s t a
create0 constructor = do
  s <- get
  case access s of
    Just b  -> pure b
    Nothing -> storeT =<< lift constructor

create1 :: ( Monad t
           , Register s b
           , MakeT s t a
           )
        => (a -> t b)
        -> StateT s t b
create1 constructor = do
  s <- get
  case access s of
    Just b  -> pure b
    Nothing -> storeT =<< join (lift . constructor <$> makeT)

create2 :: ( Monad t
           , Register s b
           , MakeT s t a1
           , MakeT s t a2
           )
        => (a1 -> a2 -> t b)
        -> StateT s t b
create2 constructor = do
  s <- get
  case access s of
    Just b  -> pure b
    Nothing -> storeT =<< join (lift ... constructor <$> makeT <*> makeT)

create3 :: ( Monad t
           , Register s b
           , MakeT s t a1
           , MakeT s t a2
           , MakeT s t a3
           )
        => (a1 -> a2 -> a3 -> t b)
        -> StateT s t b
create3 constructor = do
  s <- get
  case access s of
    Just b  -> pure b
    Nothing -> storeT =<< join (lift ... constructor <$> makeT <*> makeT <*> makeT)

create4 :: ( Monad t
           , Register s b
           , MakeT s t a1
           , MakeT s t a2
           , MakeT s t a3
           , MakeT s t a4
           )
        => (a1 -> a2 -> a3 -> a4 -> t b)
        -> StateT s t b
create4 constructor = do
  s <- get
  case access s of
    Just b  -> pure b
    Nothing -> storeT =<< join (lift ... constructor <$> makeT <*> makeT <*> makeT <*> makeT)

create5 :: ( Monad t
           , Register s b
           , MakeT s t a1
           , MakeT s t a2
           , MakeT s t a3
           , MakeT s t a4
           , MakeT s t a5
           )
        => (a1 -> a2 -> a3 -> a4 -> a5 -> t b)
        -> StateT s t b
create5 constructor = do
  s <- get
  case access s of
    Just b  -> pure b
    Nothing -> storeT =<< join (lift ... constructor <$> makeT <*> makeT <*> makeT <*> makeT <*> makeT)

create6 :: ( Monad t
           , Register s b
           , MakeT s t a1
           , MakeT s t a2
           , MakeT s t a3
           , MakeT s t a4
           , MakeT s t a5
           , MakeT s t a6
           )
        => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> t b)
        -> StateT s t b
create6 constructor = do
  s <- get
  case access s of
    Just b  -> pure b
    Nothing -> storeT =<< join (lift ... constructor <$> makeT <*> makeT <*> makeT <*> makeT <*> makeT <*> makeT)

create7 :: ( Monad t
           , Register s b
           , MakeT s t a1
           , MakeT s t a2
           , MakeT s t a3
           , MakeT s t a4
           , MakeT s t a5
           , MakeT s t a6
           , MakeT s t a7
           )
        => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> t b)
        -> StateT s t b
create7 constructor = do
  s <- get
  case access s of
    Just b  -> pure b
    Nothing -> storeT =<< join (lift ... constructor <$> makeT <*> makeT <*> makeT <*> makeT <*> makeT <*> makeT <*> makeT)

create8 :: ( Monad t
           , Register s b
           , MakeT s t a1
           , MakeT s t a2
           , MakeT s t a3
           , MakeT s t a4
           , MakeT s t a5
           , MakeT s t a6
           , MakeT s t a7
           , MakeT s t a8
           )
        => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> t b)
        -> StateT s t b
create8 constructor = do
  s <- get
  case access s of
    Just b  -> pure b
    Nothing -> storeT =<< join (lift ... constructor <$> makeT <*> makeT <*> makeT <*> makeT <*> makeT <*> makeT <*> makeT <*> makeT)

create9 :: ( Monad t
           , Register s b
           , MakeT s t a1
           , MakeT s t a2
           , MakeT s t a3
           , MakeT s t a4
           , MakeT s t a5
           , MakeT s t a6
           , MakeT s t a7
           , MakeT s t a8
           , MakeT s t a9
           )
        => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> t b)
        -> StateT s t b
create9 constructor = do
  s <- get
  case access s of
    Just b  -> pure b
    Nothing -> storeT =<< join (lift ... constructor <$> makeT <*> makeT <*> makeT <*> makeT <*> makeT <*> makeT <*> makeT <*> makeT <*> makeT)
