{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds      #-}

{- |
  This module provides functions to extract
  a DOT graph (https://en.wikipedia.org/wiki/DOT_(graph_description_language)
  out of a 'Registry'.
-}
module Data.Registry.Dot (
  module D
, makeDot
, makeDotEither
, makeDotFast
, makeDotUnsafe
) where

import           Data.Registry.Internal.Dot as D
import           Data.Registry.Statistics
import           Data.Registry.Registry
import           Data.Registry.Solver
import           Protolude

-- | Make a DOT graph for a specific value `a` built from the 'Registry'
--   `a` is at the root of the graph and its children are values
--   needed to build `a`
makeDot :: forall a ins out . (Typeable a, Contains a out, Solvable ins out)
  => Registry ins out
  -> Dot
makeDot = makeDotUnsafe @a

-- | Similar to `make` but does not check if `a` can be made out of the Regisry
--   You can use this version to get faster compilation times
makeDotFast :: forall a ins out . (Typeable a, Contains a out)
  => Registry ins out
  -> Dot
makeDotFast = makeDotUnsafe @a

-- | Similar to `make` but does not check if `a` can be made out of the 'Regisry'
--   It returns a Left value if that's not the case
makeDotEither :: forall a ins out . (Typeable a) => Registry ins out -> Either Text Dot
makeDotEither r = toDot . operations <$> makeStatisticsEither @a r

-- | Similar to `make` but does not check if `a` can be made out of the 'Regisry'
--   and throws an exception if that's not the case
makeDotUnsafe :: forall a ins out . (Typeable a) => Registry ins out -> Dot
makeDotUnsafe = toDot . operations . makeStatisticsUnsafe @a
