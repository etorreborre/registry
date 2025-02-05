{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tutorial.DatabaseLogger where

import Protolude hiding (log)

-- | A Logger interface
data Logger = Logger
  { log :: Text -> Severity -> IO ()
  }

data Severity = Info | Error | Fatal
  deriving (Eq, Ord, Show)

-- | The production implementation
newLogger :: Logger
newLogger = Logger (\t s -> print ("[" <> show s <> "] " <> t))

-- | 2 different ways to limit the severity for a Logger
limitSeverity :: Severity -> Logger -> Logger
limitSeverity at (Logger p) =
  Logger (\t s ->
    if s >= at then p t s
    else pure ())

newLimitedLogger :: Severity -> Logger
newLimitedLogger at = limitSeverity at newLogger

-- | This Logger doesn't log anything
noLogger :: Logger
noLogger = Logger (\_ _ -> pure ())

-- | A database interface
data Database = Database
  { executeQuery :: Text -> IO ()
  }

-- | A production database using a Logger
newDatabase :: Logger -> Database
newDatabase logger = Database {..} where
  executeQuery :: Text -> IO ()
  executeQuery q = do
    log logger ("executing query " <> q) Info
    print "do it"
