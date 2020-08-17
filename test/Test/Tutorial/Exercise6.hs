{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise6 where

import           Data.Registry
import           Protolude
import           System.Directory          (doesFileExist)
import           Test.Tutorial.Application

newCheckedSecretReader :: SecretReaderConfig -> Logger IO -> Tag "unchecked" (SecretReader IO) -> IO (SecretReader IO)
newCheckedSecretReader (SecretReaderConfig path) logger uncheckedReader = do
  exists <- doesFileExist (toS path)
  if not exists then fileDoesNotExist else pure ()
  pure $ unTag uncheckedReader
  where
    fileDoesNotExist = error logger ("file does not exist at " <> path)

registryIO :: Registry _ _
registryIO =
     funTo @IO App
  <: funTo @IO newLogger
  <: funTo @IO newConsole
  <: funTo @IO newUserInput
  <: funTo @IO newRng
  <: funTo @IO newCheckedSecretReader
  <: funTo @IO (tag @"unchecked" newSecretReader)
  <: valTo @IO (SecretReaderConfig "txe/tests/Test/Tutorial/secret.txt")

newAppIO :: IO App
newAppIO = make @(IO App) registryIO
