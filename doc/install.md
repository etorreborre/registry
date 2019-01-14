# Installation

This library is available on [Hackage](https://hackage.haskell.org/package/registry) and [Stackage](https://www.stackage.org/lts-13.3/package/registry-0.1.2.2) but you can also use its latest development version with [stack](https://docs.haskellstack.org/en/stable/README):

In your `stack.yaml` file put the following in the `extra-deps` section
```
extra-deps:
  - git: https://github.com/etorreborre/registry.git
    commit: 04d506eb2dbb349255d7229b37d0ab4486b97025
```

Where the commit SHA is the latest version on master.

Then you can start writing your first components and wire them:
```haskell
{-# LANGUAGE TypeApplications #-}

module RegistryTest where

import Data.Registry
import Protolude

data Logging = Logging {
  info :: Text -> IO ()
}

data LoggingConfig = LoggingConfig { logIt :: Boolean }

newLogging :: Logging = Logging print

-- |
registry =
     val (LoggingConfig True)
  +: fun newLogging
  +: end

logger = make @Logging registry
silentLogger = make @Logging (val (LoggingConfig False) +: registry)

testItAndPrintTheOutput :: IO ()
testItAndPrintTheOutput = (logger & info) "hello world"

testItAndDontPrintAnything :: IO ()
testItAndDontPrintAnything = (logger & info) "hello world"
```
