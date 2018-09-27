{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-
  This module shows how to define a Module filling in
    the role of a typeclass as required by another library.

  For example you might use a library requiring `MonadRandom`.
  How can you defined a Random module letting you use your library?
-}
module Test.Data.Registry.MonadRandomSpec where

import           Control.Monad.Random.Class      as R
import           Control.Monad.Trans.Random.Lazy
import           Data.IORef
import           Data.List
import           Data.Registry
import           Protolude                       as P
import           System.Random                   as R
import           Test.Tasty.Extensions

-- This function comes from a library
useMonadRandom :: R.MonadRandom m => m Int
useMonadRandom = R.getRandom


-- This is a module using the random generator and need the
-- useMonadRandom function from the library
data ClientModule = ClientModule { runClient :: IO Int }

newClientModule :: RandomGenerator -> ClientModule
newClientModule RandomGenerator {..} = ClientModule { runClient = runRandom useMonadRandom }

-- This is the RandomGenerator module
-- it reuses the RandT monad which "implements" MonadRandom given a specific generator
data RandomGenerator = forall g . RandomGen g => RandomGenerator {
  runRandom :: forall a . RandT g IO a -> IO a
}

-- | Production Random generator module using the global StdGen
newRandomGenerator :: IO RandomGenerator
newRandomGenerator = newStdGen >>= makeRandomGenerator

-- | This should better use a MVar when using this module concurrently
makeRandomGenerator :: (RandomGen g) => g -> IO RandomGenerator
makeRandomGenerator gen = do
  ref <- newIORef gen
  pure $ RandomGenerator (\a ->
    do g <- readIORef ref
       (r, g') <- runRandT a g
       _ <- writeIORef ref g'
       pure r)

-- | Configuration for generators returning pre-determined values
data RandomGeneratorConfig = RandomGeneratorConfig {
  seed :: Int
} deriving (Eq, Show)

-- | All the values for this generator are deterministic and determined by
--   the seed in the configuration
newSeededRandomGenerator :: RandomGeneratorConfig -> IO RandomGenerator
newSeededRandomGenerator (RandomGeneratorConfig aSeed) = do
  makeRandomGenerator (mkStdGen aSeed)

-- | There is only one value for this generator determined by
--   the seed in the configuration
newFixedRandomGenerator :: RandomGeneratorConfig -> RandomGenerator
newFixedRandomGenerator (RandomGeneratorConfig aSeed) =
  RandomGenerator ((fst <$>) . flip runRandT (mkStdGen aSeed))

registryProd =
      funTo @IO newClientModule
   +: fun newRandomGenerator
   +: end

test_client_function_with_random_values = test "a function using MonadRandom can be executed with the RandomGenerator module and return random values" $ do
  client  <- liftIO $ make @(IO ClientModule) registryProd
  results <- liftIO $ replicateM 10 $ client & runClient

  annotateShow results

  -- if we call the generator several times we should get at least 2 different values
  assert (length (nub results) > 2)

test_client_function_with_seeded_values = test "a function using MonadRandom can be executed with the RandomGenerator module and return predetermined values" $ do
  let registry' =
          funAs @IO (newSeededRandomGenerator (RandomGeneratorConfig 1))
       +: registryProd

  client  <- liftIO $ make @(IO ClientModule) registry'
  results <- liftIO $ replicateM 10 $ client & runClient

  annotateShow results

  -- everytime we call the generator we get different values but the same list
  take 3 results === [7918028818325808681, 3944251743029676875, 4139876178697185090]

test_client_function_with_fixed_values = test "a function using MonadRandom can be executed with the RandomGenerator module can return always the same value" $ do
  let registry' =
          funTo @IO (newFixedRandomGenerator (RandomGeneratorConfig 1))
       +: registryProd

  client  <- liftIO $ make @(IO ClientModule) registry'
  results <- liftIO $ replicateM 10 $ client & runClient

  annotateShow results

  -- everytime we call the generator we get the same value
  length (nub results) === 1


----
tests = $(testGroupGenerator)
