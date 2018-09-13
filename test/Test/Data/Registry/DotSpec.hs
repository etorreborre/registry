{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.DotSpec where

import           Data.Registry.Dot
import           Data.Registry
import           Protolude
import Data.Text as T
import           Test.Tasty.Extensions

test_dot =
  prop "a dot graph can be generated from a registry" $ do
    let dot = makeDot @(IO Listener) registry
    unDot dot === T.unlines [
       "strict digraph {"
      ,"  node [shape=record]"
      ,"\"IO Test.Data.Registry.DotSpec.Listener\" -> \"Test.Data.Registry.DotSpec.ListenerConfig\\nListenerConfig nyc\";"
      ,"\"IO Test.Data.Registry.DotSpec.Listener\" -> \"IO Test.Data.Registry.DotSpec.Logging\";"
      ,"}"
      ]


-- * Helpers

-- a registry

config =
     valTo @IO (AuthConfig "auth")
  +: valTo @IO (ListenerConfig "nyc")
  +: end

registry =
     fun       newLogging
  +: funTo @IO newAuth
  +: funAs @IO newListener
  +: config

-- A small graph of components

newtype Logging = Logging { info :: Text -> IO () }

newLogging :: IO Logging
newLogging = pure (Logging print)

newtype Auth = Auth { auth :: Text -> IO Bool }
newtype AuthConfig = AuthConfig Text deriving (Eq, Show)

newAuth :: AuthConfig -> Logging -> Auth
newAuth _config _logging = Auth (\t -> print t >> pure True)

newtype Listener = Listener { listen :: Text -> IO () }
newtype ListenerConfig = ListenerConfig Text deriving (Eq, Show)

newListener :: ListenerConfig -> Logging -> IO Listener
newListener _config _logging = pure (Listener print)

----
tests = $(testGroupGenerator)
