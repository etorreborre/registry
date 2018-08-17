{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-
  This box contains an example of datatypes
  and their generators

  Note that they are defined independently from
  any datatype which would group them all.

  This means that they can be put in different packages easily
-}
module Test.Data.Gen.Generators where

import           Hedgehog
import           Hedgehog.Gen
import           Hedgehog.Range
import           Protolude          hiding (M1, list)
import           Test.Data.Gen.Make

-- * Data types and generators definitions

data M1 = M1
  { m1M2 :: M2
  , m1M3 :: [M3]
  } deriving (Show)

newtype M2 = M2 { m2M4 :: M4 } deriving (Show)
newtype M3 = M3 { m3M4 :: M4 } deriving (Show)

newtype M4 = M4 { counter :: Int } deriving (Show)

-- | define generators as instances
--   depending on other instances

instance ( Register s (Gen M1)
         , Make s (Gen M2)
         , Make s (Gen M3)) => Make s (Gen M1) where
  make = make2 $ \genM2 genM3 ->
           M1 <$> genM2 <*> list (linear 0 3) genM3

instance (Register s (Gen M2), Make s (Gen M4)) => Make s (Gen M2) where
  make = create1 M2

instance (Register s (Gen M3), Make s (Gen M4)) => Make s (Gen M3) where
  make = create1 M3

instance Register s (Gen M4) => Make s (Gen M4) where
  make = create0 $ M4 <$> element [1..10]
