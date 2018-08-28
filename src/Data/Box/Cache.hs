module Data.Box.Cache where

import           Data.Typeable           (Typeable)
import           Protolude as P

-- | A thread-safe write-once cache. If you need more functionality,
-- (e.g. multiple write, cache clearing) use an 'MVar' instead.
newtype Cache a = Cache (MVar (Maybe a))
  deriving (Eq, Typeable)

-- | Fetch the value stored in the cache,
-- or call the supplied fallback and store the result,
-- if the cache is empty.
fetch :: forall a m . (MonadIO m) => Cache a -> m a -> m a
fetch (Cache var) action =
  do m <- liftIO $ P.readMVar var
     case m of
       Just a -> pure a

       Nothing -> do
         val <- action
         liftIO $ modifyMVar_ var (\_ -> pure (Just val))
         pure val

-- | Create an empty cache.
newCache :: IO (Cache a)
newCache = do
  var <- P.newMVar Nothing
  return (Cache var)
