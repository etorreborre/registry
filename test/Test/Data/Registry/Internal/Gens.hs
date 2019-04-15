{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -fno-warn-missing-monadfail-instances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Internal.Gens where

import           Data.Registry
import           Data.Registry.TH
import           Hedgehog
import           Protolude
import           Test.Data.Registry.Internal.GensRegistry

-- Hedgehog generators for the internal types
registry = $(checkRegistry 'gensRegistry)

forall :: forall a . _ => PropertyT IO a
forall = forAll $ gen @a

gen :: forall a . _ => Gen a
gen = makeFast registry
