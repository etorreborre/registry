{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Data.Box.Logging (
  Box(..)
, Config (..)
, LoggingEnv (..)
, logInfoWith
, new
, noLogging
) where

import           Data.Box.Make
import           Data.Box.RIO
import           Prelude       (show)
import           Protolude     as P

data Box =
  Box
  { logInfo :: Text -> RIO ()
  }

instance Show Box where
  show _ = "logging box"

newtype Config =
  Config {
    scribe :: Text -> IO ()
  }

instance Show Config where
  show _ = "logging config"

newtype LoggingEnv = LoggingEnv Text

instance (Register s Box, Make s Config) => Make s Box where
  make = create1 new

-- | Let's make as if creating the box would have some
--   effect like creating a timestamped internal logger
--   sharing timestamps
new :: Config -> BoxRIO Box
new (Config loggingScribe) =
  pure $ Box
    l
  where
    l a = RIO (\e -> logInfoWith loggingScribe e a)

noLogging :: Box
noLogging = Box (const (pure ()))

logInfoWith :: (Text -> IO ()) -> Env -> Text -> IO ()
logInfoWith loggingScribe env a =
  loggingScribe $ P.show env <> ": " <> a
