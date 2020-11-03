{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Test.Data.Registry.RegistrySpec where

import           Data.IORef
import           Data.Registry
import           Protolude             as P
import           Test.Tasty.Extensions

test_create_value_with_no_args_constructor = prop "no args constructors are considered as functions" $ do
  ref         <- liftIO $ newIORef ("" :: Text)
  let registry' = funTo @IO refLogger +: funTo @IO ref +: registry

  Logger {..} <- liftIO $ make @(IO Logger) registry'
  liftIO $ info "hey"

  result <- liftIO $ readIORef ref
  result === "hey"

-- *

newtype Logger = Logger { info :: Text -> IO () }

newLogger :: IO Logger
newLogger = pure (Logger print)

refLogger :: IORef Text -> Logger
refLogger ref = Logger (writeIORef ref)

registry =
     fun newLogger
  +: end


-- * COMPILATION CHECK WITH THE <: operator

registry1 :: Registry '[] '[Int, Text]
registry1 = normalize $
     val (1::Int)
  <: (val ("t"::Text) +: end)
  <: (val ("t"::Text) +: end)
  <: val ("t"::Text)

-- * COMPILATION CHECK LIFTING (see #7)


a :: Int -> Int -> IO Int
a _ _ = pure 0

b :: Int ->  Int -> RIO Int
b = outTo @RIO liftIO a

c :: RIO Int -> RIO Int -> RIO Int
c = allTo @RIO b

-- here the result of outTo needs to be explicit
-- otherwise the type of d is RIO (Int -> Int -> RIO Int)
d :: RIO Int -> RIO Int -> RIO Int
d = allTo @RIO (outTo @RIO liftIO a :: Int ->  Int -> RIO Int)

-- to avoid the issue with type inference above, we can use argsTo
e :: RIO Int -> RIO Int -> RIO Int
e = argsTo @RIO (outTo @RIO liftIO a)
