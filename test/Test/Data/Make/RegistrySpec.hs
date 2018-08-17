{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MonoLocalBinds          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

{-
  Most of the tests in that file are compile-time tests
-}
module Test.Data.Make.RegistrySpec where

import           Data.Make.Make
import           Data.Make.Registry
import           Hedgehog              hiding (test)
import           Prelude               (String)
import           Protolude             hiding (C1, D1, M1)
import           Test.Tasty
import           Test.Tasty.Extensions
import           Test.Tasty.TH

-- | Create some components for testing
newtype A4 = A4 Int   deriving (Eq, Show)
newtype A3 = A3 A4    deriving (Eq, Show)
newtype A2 = A2 A4    deriving (Eq, Show)
data    A1 = A1 A2 A3 deriving (Eq, Show)

a4 = A4 0
a3 = A3 a4
a2 = A2 a4
a1 = A1 a2 a3

newtype M1 = M1 Int deriving (Eq, Show)
newtype M2 = M2 Int deriving (Eq, Show)
newtype M3 = M3 Int deriving (Eq, Show)
newtype M4 = M4 Int deriving (Eq, Show)

m4 = M4 0
m3 = M3 0
m2 = M2 0
m1 = M1 0

-- create registries
r24 :: Registry '[A2, A4] '[]
r24 = registry

r13 :: Registry '[A1, A3] '[]
r13 = registry

rAll:: Registry '[A1, A2, A3, A4] '[]
rAll = registry

-- | set a value on a registry
rAll' :: Registry '[A1, A2, A3, A4] '[]
rAll' = override (A4 1) rAll

mAll:: Registry '[A1, A2, A3, A4] '[M1, M2, M3, M4]
mAll = m1 +: m2 +: m3 +: m4 +: rAll

test_override = test "it is possible to override a value" $ do
  -- | override an empty registry at different places
  getFrom (override a1 rAll) === Just a1
  getFrom (override a2 rAll) === Just a2
  getFrom (override a3 rAll) === Just a3
  getFrom (override a4 rAll) === Just a4

  -- | override an empty registry at different mandatory places
  getFrom (override (M1 1) mAll) === Just (M1 1)
  getFrom (override (M2 1) mAll) === Just (M2 1)
  getFrom (override (M3 1) mAll) === Just (M3 1)
  getFrom (override (M4 1) mAll) === Just (M4 1)

  -- | override a repository with a value set in only one place
  getFrom (override a1 (a1 +: (registry :: Registry '[A2, A3, A4] '[]))) === Just a1
  getFrom (override a2 ((registry :: Registry '[A1] '[]) `add`
                        (a2 +: (registry :: Registry '[A3, A4] '[])))) === Just a2
  getFrom (override a3 ((registry :: Registry '[A1, A2] '[]) `add`
                        (a3 +: (registry :: Registry '[A4] '[])))) === Just a3
  getFrom (override a4 ((registry :: Registry '[A1, A2, A3] '[]) `add`
                        (a4 +: end))) === Just a4

  -- | override a repository with a value set in only one mandatory place
  getFrom (override (M1 1) (a1 +: mAll)) === Just (M1 1)
  getFrom (override (M2 1) (override a2 mAll)) === Just (M2 1)
  getFrom (override (M3 1) (override a3 mAll)) === Just (M3 1)
  getFrom (override (M4 1) (override a4 mAll)) === Just (M4 1)


-- | check that we can make components given a registry
makeA1 :: A1
makeA1 = evalState make rAll

makeA1' :: A1
makeA1' = evalState make rAll'

-- | check that components are properly overridden
test_overridden1 = test "test that a component is overridden" $
  case makeA1' of
    A1 (A2 (A4 x)) (A3 (A4 y)) ->
      do x === y
         x === 1

test_overridden2 = test "test that a component is overridden in the middle of the list" $ do
  getFrom (a1 +: a2 +: a3 +: a4 +: end) === Just a1
  getFrom (a1 +: a2 +: a3 +: a4 +: end) === Just a2
  getFrom (a1 +: a2 +: a3 +: a4 +: end) === Just a3
  getFrom (a1 +: a2 +: a3 +: a4 +: end) === Just a4

instance Register s A4 => Make s A4 where
  make = state $ \r ->
    case access r of
      Just a2' -> (a2', r)
      Nothing ->
        let a4' = A4 0
            r' = register a4' r
        in (a4', r')

instance (Register s A2, Make s A4) => Make s A2 where
  make = state $ \r ->
    case access r of
      Just a2' -> (a2', r)
      Nothing ->
        let (a4', r') = runState make r
            r'' = register (A2 a4') r'
        in (A2 a4', r'')

instance (Register s A3, Make s A4) => Make s A3 where
  make = state $ \r ->
    case access r of
      Just a3' -> (a3', r)
      Nothing ->
        let (a4', r') = runState make r
            r'' = register (A3 a4') r'
        in (A3 a4', r'')

instance (Register s A1, Make s A2, Make s A3) => Make s A1 where
  make = state $ \r ->
    case access r of
      Just a1' -> (a1', r)
      Nothing ->
        let (a1', r') = runState (do
                         a2' <- make
                         a3' <- make
                         pure $ A1 a2' a3') r
            r'' = register a1' r'
        in (a1', r'')

-- * Test the union of registries having a various number of
-- optional and mandatory types
newtype Config1 = Config1 Int
newtype Config2 = Config2 Text

type AppRegistry = Registry '[A1, A2, A3, A4] '[Config1, Config2]

newtype C1 = C1 Int  deriving (Eq, Show)
newtype C2 = C2 Text deriving (Eq, Show)

-- Define a configuration
config :: Registry '[] '[C1, C2]
config =
  C1 1 +:
  C2 "x" +:
  end

-- | try some overrides
registryAndConfig :: Registry '[A1, A2, A3, A4] '[C1, C2]
registryAndConfig = rAll `add` config

registryAndConfig1 :: Registry '[A1, A2, A3, A4] '[C1, C2]
registryAndConfig1 = override (A1 (A2 (A4 1)) (A3 (A4 1))) registryAndConfig

registryAndConfig2 :: Registry '[A1, A2, A3, A4] '[C1, C2]
registryAndConfig2 = override (A4 1) registryAndConfig

registryAndConfig3 :: Registry '[A1, A2, A3, A4] '[C1, C2]
registryAndConfig3 = override (C1 1) registryAndConfig

registryAndConfig4 :: Registry '[A1, A2, A3, A4] '[C1, C2]
registryAndConfig4 = override (C2 "1") registryAndConfig

registryAndConfig5 :: Registry '[] '[C1, C2]
registryAndConfig5 = override (C2 "1") config

registryAndConfig6 :: Registry '[A4] '[]
registryAndConfig6 = override (A4 1) (registry :: Registry '[A4] '[])

registryAndConfig7 :: Registry '[A1, A2, A3, A4] '[C1, C2]
registryAndConfig7 = override (A3 (A4 1)) registryAndConfig

-- | load test the union of registries
--   it should not take too much time to compile
newtype X0  = X0  Int
newtype X1  = X1  Int
newtype X2  = X2  Int
newtype X3  = X3  Int
newtype X4  = X4  Int
newtype X5  = X5  Int
newtype X6  = X6  Int
newtype X7  = X7  Int
newtype X8  = X8  Int
newtype X9  = X9  Int
newtype X10 = X10 Int
newtype X11 = X11 Int
newtype X12 = X12 Int
newtype X13 = X13 Int
newtype X14 = X14 Int
newtype X15 = X15 Int
newtype X16 = X16 Int
newtype X17 = X17 Int
newtype X18 = X18 Int
newtype X19 = X19 Int


-- | Try to simply add registries together without
--   deduplicating types
--   Compilation times are ok
type A0_to_9 = '[
    X0
  , X1
  , X2
  , X3
  , X4
  , X6
  , X7
  , X8
  , X9
  ]

type A10_to_19 = '[
    X10
  , X11
  , X12
  , X13
  , X14
  , X15
  , X16
  , X17
  , X18
  , X19
  ]

aX_0_9 :: Registry A0_to_9 '[]
aX_0_9 = registry

aX_10_19 :: Registry A10_to_19 '[]
aX_10_19 = registry

ax_0_9_twice = aX_0_9 `add` aX_0_9
ax_0_19      = aX_0_9 `add` aX_10_19

-- * Test the Show instance for Registry
--   it should display only the mandatory values
test_show = test "show mandatory values" $
  show registryAndConfig7 === ("[C1 1\nC2 \"x\"\n]" :: String)

tests = $(testGroupGenerator)
