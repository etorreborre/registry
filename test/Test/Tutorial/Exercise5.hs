{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise5 where

import qualified Data.ByteString.Char8 as BS8
import Data.Registry
import Protolude
import System.Directory (doesFileExist)
import Test.Tutorial.Application

newCheckedSecretReader :: SecretReaderConfig -> Logger IO -> IO (SecretReader IO)
newCheckedSecretReader (SecretReaderConfig path) logger = do
  exists <- doesFileExist (toS path)
  if not exists then fileDoesNotExist else pure ()
  pure
    SecretReader
      { readSecret =
          if exists
            then Just . decodeUtf8 <$> BS8.readFile (toS path)
            else fileDoesNotExist $> Nothing
      }
  where
    fileDoesNotExist = error logger ("file does not exist at " <> path)

registryIO :: Registry _ _
registryIO =
  funTo @IO App
    <: funTo @IO newUserInput
    <: funTo @IO newRng
    <: funTo @IO newCheckedSecretReader
    <: funTo @IO newLogger
    <: funTo @IO newConsole
    <: valTo @IO (SecretReaderConfig "txe/tests/Test/Tutorial/secret.txt")

newAppIO :: IO App
newAppIO = make @(IO App) registryIO
