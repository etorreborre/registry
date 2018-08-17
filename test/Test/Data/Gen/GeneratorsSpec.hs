{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Gen.GeneratorsSpec where

import           Control.Lens             hiding (element)
import           Hedgehog                 hiding (test)
import           Protolude                hiding (M1, list)
import           Test.Data.Gen.Generators
import           Test.Data.Gen.Make
import           Test.Tasty
import           Test.Tasty.Extensions
import           Test.Tasty.TH


-- A Gens datatype holding default definitions
-- for generators for all these types is created here
makeGens [''M1, ''M2, ''M3, ''M4]

test_generators1 = test "use default generators" $ do
  -- then we can invoke the generator for a given type
  -- just with a type application
  value1 <- forAll $ gen @M1
  let n = value1 & m1M2 & m2M4 & counter
  (n >= 1 && n <= 10) === True

test_generators2 = test "use an overridden generator" $ do
  -- if needed we can override an existing generator with
  -- another one
  let overridden = gensDef { genM4 = Just (pure (M4 0)) }

  -- here instead of using the generic 'gen' we call 'makeGen'
  -- with a specific Gens state
  value1 <- forAll $ makeGen overridden
  let n = value1 & m1M2 & m2M4 & counter
  n === 0

tests = $(testGroupGenerator)
