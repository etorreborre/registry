{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.Exercise7 where

import           Data.Registry
import           Protolude
import           Test.Tutorial.Application
import           Test.Tutorial.Exercise6

newInitializedLogger :: IO (Logger IO)
newInitializedLogger = do
  print ("start the logger" :: Text)
  pure (Logger putStrLn putStrLn)

newInitializedRegistry :: Registry _ _
newInitializedRegistry = fun newInitializedLogger +: registryIO

newInitializedAppIO :: IO App
newInitializedAppIO = make @(IO App) newInitializedRegistry

memoizedRegistry :: IO (Registry _ _)
memoizedRegistry = memoize @IO @(Logger IO) newInitializedRegistry

newInitializedMemoizedAppIO :: IO App
newInitializedMemoizedAppIO =  make @(IO App) =<< memoizedRegistry

memoizedAllRegistry :: IO (Registry _ _)
memoizedAllRegistry = memoizeAll @IO newInitializedRegistry

newInitializedMemoizedAllAppIO :: IO App
newInitializedMemoizedAllAppIO =  make @(IO App) =<< memoizedAllRegistry
