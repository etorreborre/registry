{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Data.Registry.THSpec where

import           Data.Generics.Product.Typed
import           Data.Registry
import           Data.Registry.TH
import           Protolude
import           Universum                   ((...))

-- * Example of TemplateHaskell usage to create the typeclass corresponding to a component

-- | We define 2 low-level components: Logger and Tracer
--   We make the corresponding typeclasses
data Logger m = Logger {
  -- a function taking 2 arguments
  _info  :: Text -> Text -> m ()
  -- a function having a constraint
, _error :: forall a . (Monoid a) => a -> Text -> m ()
}

makeTypeclass ''Logger

newtype Tracer m = Tracer {
  _traceIt :: Text -> m ()
}

makeTypeclass ''Tracer

-- | Now we make a component which is going to use both components
newtype Service m = Service {
  doIt :: Int -> Text -> m ()
}

newService :: Monad m => Logger m -> Tracer m -> Service m
newService logger tracer = Service {
  doIt = run ... implementService
} where
    -- the typeclass instances can be resolved by using a Reader containing
    --  a tuple with all the components
    run = flip runReaderT (logger, tracer)

-- | Implement the service using typeclasses
implementService ::
     Monad m
  => WithLogger m
  => WithTracer m
  => Int
  -> Text
  -> m ()
implementService n t = do
  info "doing it" t
  traceIt (show n)

-- * Example of TemplateHaskell usage to check the completeness of a registry

add0 :: Int -> Text
add0 _ = ""

times2 :: Double -> Text
times2 n = show (n * 2)

reg0 :: Registry '[Int] '[Text, Int, Int]
reg0 = fun add0 +: val (1::Int) +: val (2 :: Int) +: end

-- See the gory details of why this is necessary: https://gitlab.haskell.org/ghc/ghc/issues/9813
$(return [])

-- | this compiles ok and de-duplicates types
reg1 :: Registry '[Int] '[Text, Int]
reg1 = $(checkRegistry 'reg0)

-- Then we can use makeFast
value = makeFast @Int reg1

regIncomplete :: Registry [Double, Int] [Text, Text, Int, Int]
regIncomplete = fun times2 +: reg0

-- | This does not compile
-- reg2 :: Registry [Double, Int] [Text, Int]
-- reg2 = $(checkRegistry 'regIncomplete)

-- | Using the checkRegistry function with IO functions
addIO0 :: Int -> IO Text
addIO0 _ = pure ""

regIO0 :: Registry '[IO Int] '[IO Text, IO Int, IO Int]
regIO0 = funAs @IO addIO0 +: valTo @IO (1 :: Int) +: valTo @IO (2 :: Int) +: end

-- See the gory details of why this is necessary: https://gitlab.haskell.org/ghc/ghc/issues/9813
$(return [])

regIO1 :: Registry '[IO Int] '[IO Text, IO Int]
regIO1 = $(checkRegistry 'regIO0)

-- Then we can use makeFast
valueIO = makeFast @(IO Int) regIO1
