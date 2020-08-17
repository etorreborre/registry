{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise2 where

import           Test.Tutorial.Application
import           Data.Registry

registry :: Registry _ _
registry =
     fun App
  <: fun newLogger
  <: fun newConsole
  <: fun newUserInput
  <: fun newRng
  <: fun newSecretReader
  <: val (SecretReaderConfig "txe/tests/Test/Tutorial/secret.txt")

newApp :: App
newApp = make @App registry
