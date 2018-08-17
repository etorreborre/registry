{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-
  This module helps with the creation of dependent boxes
-}
module Data.Make.Make (
  module Register
, Make(..)
, getInstance
, makeInstance
, store
-- make functions are for making boxes from a Register
-- where each dependency can be created effectfully
, make0
, make1
, make2
, make3
, make4
, make5
, make6
, make7
, make8
, make9
, make10
, make11
, make12
, make13
-- create functions are for making boxes from a Register
-- where each dependency can be created purely
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
, create10
, create11
, create12
, create13
) where

import           Data.Default
import           Data.Make.Register as Register
import           Protolude

-- | Typeclass making and storing an object
class Make s a where
  make :: State s a

-- | Get a given instance from the default state
getInstance :: forall s a . (Default s, Make s a) => a
getInstance = makeInstance (def @s)

-- | Make a given instance with a specific state
makeInstance :: forall s a . (Make s a) => s -> a
makeInstance = evalState make

-- | Register a given object and return it
store :: (Register s a) => a -> State s a
store g = modify (register g) >> pure g

make0 :: (Register s a) => a -> State s a
make0 constructor = do
  s <- get
  case access s of
    Just m  -> pure m
    Nothing -> store constructor

make1 :: ( Functor m
         , Register s (m b)
         , Make s (m a)
         )
        => (m a -> m b)
        -> State s (m b)
make1 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> store =<< constructor <$> make

make2 :: ( Functor m
         , Register s (m b)
         , Make s (m a1)
         , Make s (m a2)
         )
        => (m a1 -> m a2 -> m b)
        -> State s (m b)
make2 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> store =<< constructor <$> make <*> make

make3 :: ( Functor m
         , Register s (m b)
         , Make s (m a1)
         , Make s (m a2)
         , Make s (m a3)
         )
        => (m a1 -> m a2 -> m a3 -> m b)
        -> State s (m b)
make3 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> store =<< constructor <$> make <*> make <*> make

make4 :: ( Functor m
         , Register s (m b)
         , Make s (m a1)
         , Make s (m a2)
         , Make s (m a3)
         , Make s (m a4)
         )
        => (m a1 -> m a2 -> m a3 -> m a4 -> m b)
        -> State s (m b)
make4 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> store =<< constructor <$> make <*> make <*> make <*> make

make5 :: ( Functor m
         , Register s (m b)
         , Make s (m a1)
         , Make s (m a2)
         , Make s (m a3)
         , Make s (m a4)
         , Make s (m a5)
         )
        => (m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m b)
        -> State s (m b)
make5 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> store =<< constructor <$> make <*> make <*> make <*> make <*> make

make6 :: ( Functor m
         , Register s (m b)
         , Make s (m a1)
         , Make s (m a2)
         , Make s (m a3)
         , Make s (m a4)
         , Make s (m a5)
         , Make s (m a6)
         )
        => (m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m b)
        -> State s (m b)
make6 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> store =<< constructor <$> make <*> make <*> make <*> make <*> make <*> make

make7 :: ( Functor m
         , Register s (m b)
         , Make s (m a1)
         , Make s (m a2)
         , Make s (m a3)
         , Make s (m a4)
         , Make s (m a5)
         , Make s (m a6)
         , Make s (m a7)
         )
        => (m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m b)
        -> State s (m b)
make7 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> store =<< constructor <$> make <*> make <*> make <*> make <*> make <*> make <*> make

make8 :: ( Functor m
         , Register s (m b)
         , Make s (m a1)
         , Make s (m a2)
         , Make s (m a3)
         , Make s (m a4)
         , Make s (m a5)
         , Make s (m a6)
         , Make s (m a7)
         , Make s (m a8)
         )
        => (m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m b)
        -> State s (m b)
make8 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> store =<< constructor <$> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make

make9 :: ( Functor m
         , Register s (m b)
         , Make s (m a1)
         , Make s (m a2)
         , Make s (m a3)
         , Make s (m a4)
         , Make s (m a5)
         , Make s (m a6)
         , Make s (m a7)
         , Make s (m a8)
         , Make s (m a9)
         )
        => (m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m a9 -> m b)
        -> State s (m b)
make9 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> store =<< constructor <$> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make

make10 :: ( Functor m
         , Register s (m b)
         , Make s (m a1)
         , Make s (m a2)
         , Make s (m a3)
         , Make s (m a4)
         , Make s (m a5)
         , Make s (m a6)
         , Make s (m a7)
         , Make s (m a8)
         , Make s (m a9)
         , Make s (m a10)
         )
        => (m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m a9 -> m a10 -> m b)
        -> State s (m b)
make10 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> store =<< constructor <$> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make

make11 :: ( Functor m
         , Register s (m b)
         , Make s (m a1)
         , Make s (m a2)
         , Make s (m a3)
         , Make s (m a4)
         , Make s (m a5)
         , Make s (m a6)
         , Make s (m a7)
         , Make s (m a8)
         , Make s (m a9)
         , Make s (m a10)
         , Make s (m a11)
         )
        => (m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m a9 -> m a10 -> m a11 -> m b)
        -> State s (m b)
make11 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> store =<< constructor <$> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make

make12 :: ( Functor m
         , Register s (m b)
         , Make s (m a1)
         , Make s (m a2)
         , Make s (m a3)
         , Make s (m a4)
         , Make s (m a5)
         , Make s (m a6)
         , Make s (m a7)
         , Make s (m a8)
         , Make s (m a9)
         , Make s (m a10)
         , Make s (m a11)
         , Make s (m a12)
         )
        => (m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m a9 -> m a10 -> m a11 -> m a12 -> m b)
        -> State s (m b)
make12 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> store =<< constructor <$> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make

make13 :: ( Functor m
         , Register s (m b)
         , Make s (m a1)
         , Make s (m a2)
         , Make s (m a3)
         , Make s (m a4)
         , Make s (m a5)
         , Make s (m a6)
         , Make s (m a7)
         , Make s (m a8)
         , Make s (m a9)
         , Make s (m a10)
         , Make s (m a11)
         , Make s (m a12)
         , Make s (m a13)
         )
        => (m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m a9 -> m a10 -> m a11 -> m a12 -> m a13 -> m b)
        -> State s (m b)
make13 constructor = do
  s <- get
  case access s of
    Just mb -> pure mb
    Nothing -> store =<< constructor <$> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make <*> make

create0 :: ( Register s (m a)
           , Make s (m a)
           )
        => m a
        -> State s (m a)
create0 = make0

create1 :: ( Functor m
           , Register s (m b)
           , Make s (m a)
           )
        => (a -> b)
        -> State s (m b)
create1 constructor = make1 (constructor <$>)

create2 :: ( Applicative m
           , Register s (m b)
           , Make s (m a1)
           , Make s (m a2)
           )
        => (a1 -> a2 -> b)
        -> State s (m b)
create2 constructor = make2 (\ma1 ma2 -> constructor <$> ma1 <*> ma2)

create3 :: ( Applicative m
           , Register s (m b)
           , Make s (m a1)
           , Make s (m a2)
           , Make s (m a3)
           )
        => (a1 -> a2 -> a3 -> b)
        -> State s (m b)
create3 constructor = make3 (\ma1 ma2 ma3 -> constructor <$> ma1 <*> ma2 <*> ma3)

create4 :: ( Applicative m
           , Register s (m b)
           , Make s (m a1)
           , Make s (m a2)
           , Make s (m a3)
           , Make s (m a4)
           )
        => (a1 -> a2 -> a3 -> a4 -> b)
        -> State s (m b)
create4 constructor = make4 (\ma1 ma2 ma3 ma4 -> constructor <$> ma1 <*> ma2 <*> ma3 <*> ma4)

create5 :: ( Applicative m
           , Register s (m b)
           , Make s (m a1)
           , Make s (m a2)
           , Make s (m a3)
           , Make s (m a4)
           , Make s (m a5)
           )
        => (a1 -> a2 -> a3 -> a4 -> a5 -> b)
        -> State s (m b)
create5 constructor = make5 (\ma1 ma2 ma3 ma4 ma5 -> constructor <$> ma1 <*> ma2 <*> ma3 <*> ma4 <*> ma5)

create6 :: ( Applicative m
           , Register s (m b)
           , Make s (m a1)
           , Make s (m a2)
           , Make s (m a3)
           , Make s (m a4)
           , Make s (m a5)
           , Make s (m a6)
           )
        => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b)
        -> State s (m b)
create6 constructor = make6 (\ma1 ma2 ma3 ma4 ma5 ma6 -> constructor <$> ma1 <*> ma2 <*> ma3 <*> ma4 <*> ma5 <*> ma6)

create7 :: ( Applicative m
           , Register s (m b)
           , Make s (m a1)
           , Make s (m a2)
           , Make s (m a3)
           , Make s (m a4)
           , Make s (m a5)
           , Make s (m a6)
           , Make s (m a7)
           )
        => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b)
        -> State s (m b)
create7 constructor = make7 (\ma1 ma2 ma3 ma4 ma5 ma6 ma7 -> constructor <$> ma1 <*> ma2 <*> ma3 <*> ma4 <*> ma5 <*> ma6 <*> ma7)

create8 :: ( Applicative m
           , Register s (m b)
           , Make s (m a1)
           , Make s (m a2)
           , Make s (m a3)
           , Make s (m a4)
           , Make s (m a5)
           , Make s (m a6)
           , Make s (m a7)
           , Make s (m a8)
           )
        => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> b)
        -> State s (m b)
create8 constructor = make8 (\ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8 -> constructor <$> ma1 <*> ma2 <*> ma3 <*> ma4 <*> ma5 <*> ma6 <*> ma7 <*> ma8)

create9 :: ( Applicative m
           , Register s (m b)
           , Make s (m a1)
           , Make s (m a2)
           , Make s (m a3)
           , Make s (m a4)
           , Make s (m a5)
           , Make s (m a6)
           , Make s (m a7)
           , Make s (m a8)
           , Make s (m a9)
           )
        => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> b)
        -> State s (m b)
create9 constructor = make9 (\ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8 ma9 -> constructor <$> ma1 <*> ma2 <*> ma3 <*> ma4 <*> ma5 <*> ma6 <*> ma7 <*> ma8 <*> ma9)

create10 :: ( Applicative m
            , Register s (m b)
            , Make s (m a1)
            , Make s (m a2)
            , Make s (m a3)
            , Make s (m a4)
            , Make s (m a5)
            , Make s (m a6)
            , Make s (m a7)
            , Make s (m a8)
            , Make s (m a9)
            , Make s (m a10)
            )
        => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> b)
        -> State s (m b)
create10 constructor = make10 (\ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8 ma9 ma10 -> constructor <$> ma1 <*> ma2 <*> ma3 <*> ma4 <*> ma5 <*> ma6 <*> ma7 <*> ma8 <*> ma9 <*> ma10)

create11 :: ( Applicative m
           , Register s (m b)
           , Make s (m a1)
           , Make s (m a2)
           , Make s (m a3)
           , Make s (m a4)
           , Make s (m a5)
           , Make s (m a6)
           , Make s (m a7)
           , Make s (m a8)
           , Make s (m a9)
           , Make s (m a10)
           , Make s (m a11)
           )
        => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> b)
        -> State s (m b)
create11 constructor = make11 (\ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8 ma9 ma10 ma11 -> constructor <$> ma1 <*> ma2 <*> ma3 <*> ma4 <*> ma5 <*> ma6 <*> ma7 <*> ma8 <*> ma9 <*> ma10 <*> ma11)

create12 :: ( Applicative m
            , Register s (m b)
            , Make s (m a1)
            , Make s (m a2)
            , Make s (m a3)
            , Make s (m a4)
            , Make s (m a5)
            , Make s (m a6)
            , Make s (m a7)
            , Make s (m a8)
            , Make s (m a9)
            , Make s (m a10)
            , Make s (m a11)
            , Make s (m a12)
            )
        => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> b)
        -> State s (m b)
create12 constructor = make12 (\ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8 ma9 ma10 ma11 ma12 -> constructor <$> ma1 <*> ma2 <*> ma3 <*> ma4 <*> ma5 <*> ma6 <*> ma7 <*> ma8 <*> ma9 <*> ma10 <*> ma11 <*> ma12)

create13 :: ( Applicative m
            , Register s (m b)
            , Make s (m a1)
            , Make s (m a2)
            , Make s (m a3)
            , Make s (m a4)
            , Make s (m a5)
            , Make s (m a6)
            , Make s (m a7)
            , Make s (m a8)
            , Make s (m a9)
            , Make s (m a10)
            , Make s (m a11)
            , Make s (m a12)
            , Make s (m a13)
            )
        => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> a13 -> b)
        -> State s (m b)
create13 constructor = make13 (\ma1 ma2 ma3 ma4 ma5 ma6 ma7 ma8 ma9 ma10 ma11 ma12 ma13 -> constructor <$> ma1 <*> ma2 <*> ma3 <*> ma4 <*> ma5 <*> ma6 <*> ma7 <*> ma8 <*> ma9 <*> ma10 <*> ma11 <*> ma12 <*> ma13)
