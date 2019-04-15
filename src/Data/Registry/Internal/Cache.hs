{- |

 Cache for individual IO values when we wish to memoize actions
 for database connection pools for example

 This is inspired by https://hackage.haskell.org/package/io-memoize

-}
module Data.Registry.Internal.Cache where

import           Data.Map.Strict
import           Data.Registry.Internal.Types (SpecializationPath)
import           Data.Typeable                (Typeable)
import           Protolude                    as P

-- | A thread-safe write-once cache. If you need more functionality,
-- (e.g. multiple write, cache clearing) use an 'MVar' instead.
newtype Cache a = Cache (MVar (Map Key a))
  deriving (Eq, Typeable)

-- | We need to cache different values to account for the fact
--   that different values might be specialized for the same type
type Key = Maybe [SpecializationPath]

-- | Fetch the value stored in the cache,
-- or call the supplied fallback and store the result,
-- if the cache is empty.
fetch :: forall a m . (MonadIO m, Typeable a) => Cache a -> Key -> m a -> m a
fetch (Cache var) key action =
  do m <- liftIO $ P.readMVar var
     case lookup key m of
        Just a ->
          pure a

        Nothing -> do
          val <- action
          liftIO $ modifyMVar_ var (\cached -> pure $ insert key val cached)
          pure val

-- | Create an empty cache.
newCache :: IO (Cache a)
newCache = Cache <$> P.newMVar mempty
