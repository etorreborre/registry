{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Test.Data.Registry.Internal.MakeSpec where

import           Data.Registry.Internal.Make
import           Data.Registry.Internal.Types
import           Data.Registry.Internal.Stack
import           Data.Text                        as T
import           Protolude                        as P
import           Test.Data.Registry.Internal.Gens
import           Test.Tasty.Extensions
import           Type.Reflection

test_make_inputs_with_cycle = prop "making inputs when there's a cycle must be detected" $ do
  target          <- forall @SomeTypeRep
  context'        <- forall @Context
  functions       <- forall @Functions
  specializations <- forall @Specializations
  modifiers       <- forall @Modifiers
  values          <- forall @Values

  -- put one of the input types to build already in the list of
  -- types being built
  let context = Context (target : _contextStack context')

  let result = runStack (makeInputs [target] context  functions specializations modifiers) values
  case result of
    Left e  -> annotateShow e >> "cycle detected!" `T.isPrefixOf` e === True
    Right _ -> failure


----
tests = $(testGroupGenerator)
