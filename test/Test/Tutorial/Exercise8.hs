{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise8 where

import           Data.Registry
import           Data.Registry.TH
import           Protolude
import           Test.Tutorial.Application
import           Test.Tutorial.Exercise2

erasedRegistry :: Registry '[ERASED_TYPES] '[ERASED_TYPES]
erasedRegistry = eraseTypes registry

incorrectRegistry = fun (\(_::Int) (_::Logger IO) (_:: Console IO) -> App) +: registry

checkedRegistry = $(checkRegistry 'registry)

$(return [])

-- this does not compile
-- checkedIncorrectRegistry = $(checkRegistry 'incorrectRegistry)

newErasedApp :: App
newErasedApp = makeUnsafe @App erasedRegistry
