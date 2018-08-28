module Data.Box.Cache where

import           Data.Typeable           (Typeable)
import           Protolude as P hiding (readMVar, modifyMVar_)
import           UnliftIO.MVar
import           Control.Monad.IO.Unlift

-- | A thread-safe write-once cache. If you need more functionality,
-- (e.g. multiple write, cache clearing) use an 'MVar' instead.
newtype Cache a = Cache (MVar (Maybe a))
  deriving (Eq, Typeable)

memo :: MonadUnliftIO m => m a -> IO (m a)
memo action = do
 cache <- newCache
 pure (fetch cache action)

-- | Fetch the value stored in the cache,
-- or call the supplied fallback and store the result,
-- if the cache is empty.
fetch :: forall a m . (MonadUnliftIO m) => Cache a -> m a -> m a
fetch (Cache var) = go
  where
      go :: m a -> m a
      go f = do
        m <- readMVar var
        case m of
          Just aw -> pure aw
          Nothing -> do
            modifyMVar_ var $ \case
              Just a -> pure (Just a)
              Nothing -> Just <$> f
            go f

-- | Create an empty cache.
newCache :: IO (Cache a)
newCache = do
  var <- P.newMVar Nothing
  return (Cache var)
