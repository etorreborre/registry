{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Logging where

data Logging = Logging
  { info  :: Text -> IO ()
  , warn  :: Text -> IO ()
  , error :: Text -> IO ()
  , debug :: Text -> IO ()
  }

new :: Logging
new = Logging
  { info  = print $ "[INFO] " <> t
  , warn  = print $ "[WARN] " <> t
  , error = print $ "[ERROR] " <> t
  , debug = print $ "[DEBUG] " <> t
  }

noLogging :: Logging

noLogging = Logging
  { info  = const (pure ())
  , warn  = const (pure ())
  , error = const (pure ())
  , debug = const (pure ())
  }

newLogger :: Logger
newLogger = Logger print

noLogging = Logger (const (pure ()))

module Authentication where

data Authentication = Authentication {
    authenticate :: User -> IO ()
  }

new ::
     AuthConfig
  -> Logging
  -> Authentication
new config logger = Module {
    authenticate = \user -> do
      (logger & info) $
        "authenticating " <> show user
  }

instance (
    Register s Authentication
  , Make s AuthConfig
  , Make s Logging) =>
  Make s Authentication where
  make = create2 ((pure .) . new)

new :: AuthConfig -> Logging -> Authentication

AuthConfig "./credentials"

module Logging where
  new :: IO Logging

module Metrics where
  new :: MetricsConfig
      -> IO Metrics

module Reliability where
  new :: ReliabilityConfig
      -> Logging
      -> Reliability

module Authentication where
  new :: AuthConfig
      -> Logging
      -> FileSystem
      -> Authentication

registry =
     fun       Logging.new
  +: valTo @IO (MetricsConfig "localhost" 8080)
  +: funTo @IO Reliability.new
  +: funAs @IO Metrics.new
  +: funAs @IO Authentication.new
  +: end

new :: DatabaseConfig -> IO Database

registry :: IO (Registry _ _)
registry =
  singleton @IO @Database $
       valTo @IO (DatabaseConfig "localhost" 5432)
    +: funTo @IO Database
    +: end

specialize @(IO ProductsConsumer)
           (ListenerConfig "http://nyc") $

specialize @(IO PartnersConsumer)
           (ListenerConfig "http://london") $
  registry
