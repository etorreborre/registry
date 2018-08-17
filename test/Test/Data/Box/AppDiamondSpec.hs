{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Box.AppDiamondSpec where

import           Control.Lens
import           Control.Monad.Trans.Resource hiding (allocate)
import           Data.Box.Make
import           Data.Box.RIO
import           Data.Box.Warmup
import           Data.IORef
import           Data.Make.TH                 (camelCaseTypeName)
import           Data.Text
import           Hedgehog                     hiding (eval, test)
import           Prelude                      (show)
import           Protolude                    as P hiding (M1)
import           Test.Tasty
import           Test.Tasty.Extensions
import           Test.Tasty.TH

-- * Data types
data M1 = M1
  { m1M2  :: M2
  , m1M3  :: M3
  , m1Ref :: IORef [Text]
  }

newtype M2 = M2 { m2M4 :: M4 }
newtype M3 = M3 { m3M4 :: M4 }
newtype M4 = M4 { m4Ref :: IORef [Text] }

makeRegistryWith (("_" <>) . camelCaseTypeName 1) [''M1, ''M2, ''M3, ''M4]

-- * Tests
test_warmup_box =
  test "an app can start and stop all of its components" $ do
    (_, warmupResult) <- runLiftIO (runResourceT $ runAll boxDef :: RIO (M1, Result))

    annotateShow "Both M1 and M4 are warmed up"
    warmupResult === Ok ["startM4", "startM1"]

test_cleanup_box =
  test "an app can cleanup its resources on exit" $ do
    m1 <- runLiftIO (runResourceT $ startAll boxDef :: RIO M1)

    annotateShow "M1 and M4 are cleaned up"

    r1 <- liftIO $ readIORef (m1 & m1Ref)
    r4 <- liftIO $ readIORef (m1 & m1M3 & m3M4 & m4Ref)

    r1 === ["startM1", "stopM1"]
    r4 === ["startM4", "stopM4"]

test_eval =
  test "eval must not perform any warmup or cleanup" $ do
    (m1, w, stop) <- runLiftIO (eval boxDef :: RIO (M1, Warmup, Stop))

    annotateShow "M1 and M4 are not cleaned up"

    r1 <- liftIO $ readIORef (m1 & m1Ref)
    r4 <- liftIO $ readIORef (m1 & m1M3 & m3M4 & m4Ref)

    r1 === []
    r4 === []

    annotateShow "They are warmed up when calling runWarmup"
    warmupResult <- runLiftIO $ runWarmup w

    warmupResult === Ok ["startM4", "startM1"]

    annotateShow "They are cleaned up when calling runStop"
    _ <- runLiftIO $ runStop stop

    r1' <- liftIO $ readIORef (m1 & m1Ref)
    r4' <- liftIO $ readIORef (m1 & m1M3 & m3M4 & m4Ref)

    r1' === ["startM1", "stopM1"]
    r4' === ["startM4", "stopM4"]


-- * helpers

-- | We create 4 box with the following dependencies
--    M1
--   /  \
-- M2   M3
--  \  /
--   M4
--
-- M1 and M4 have start and stop functions:
--   - M1 adds "x" to a string on start and stop
--   - M4 increments a counter by 1 on start and stop


instance Show M1 where
  show _= "m1"

-- | M1 is modelled a bit like a database component which would
--   open a connection and eventually need to close it
newM1 :: M2 -> M3 -> BoxRIO M1
newM1 m2 m3 =
  do r <- allocate (liftIO (newIORef [])) (void . useRef "stopM1")
     warmupWith (createWarmup $ useRef "startM1" r)
     pure (M1 m2 m3 r)

useRef :: Text -> IORef [Text] -> RIO Result
useRef t ref =
  do _ <- liftIO $ modifyIORef ref (++ [t])
     pure (ok t)

instance (Register s M1, Make s M2, Make s M3) => Make s M1 where
  make = create2 newM1

instance (Register s M2, Make s M4) => Make s M2 where
  make = create1 (pure . M2)

instance (Register s M3, Make s M4) => Make s M3 where
  make = create1 (pure . M3)


instance Show M4 where
  show _ = "m4"

-- | M4 is a component where start/stop functions are directly accessible from its interface
--   so we use startStop0 to create it and pass a function M4 -> StartStop
newM4 :: BoxRIO M4
newM4 =
  do r <- allocate (liftIO (newIORef [])) (void . useRef "stopM4")
     warmupWith (createWarmup $ useRef "startM4" r)
     pure (M4 r)


useM4Ref :: Text -> M4 -> RIO Result
useM4Ref t m4 =
  do _ <- liftIO $ modifyIORef (m4Ref m4) (++ [t])
     pure (ok t)

instance (Register s M4) => Make s M4 where
  make = create0 newM4


tests = $(testGroupGenerator)
