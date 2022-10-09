{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Make.MemoizeSpec where

import Data.IORef
import Data.Registry
import Protolude hiding (C1, D1)
import System.IO.Memoize
import Test.Tasty.Extensions

test_memoize = test "effectful values can be memoized with System.IO.Memoize" $ do
  (c1, c2) <- liftIO $ do
    -- create a counter for the number of instantiations
    counter <- newIORef 0

    newSingOnce <- once (newSing counter)
    let r =
          funTo @IO newC1
            <: funTo @IO newC2
            <: funTo @IO newSingOnce

    c1 <- make @(IO C1) r
    c2 <- make @(IO C2) r
    pure (c1, c2)

  c1 === C1 (Sing 1)
  c2 === C2 (Sing 1)

test_memoize_proper = test "effectful values can memoized" $ do
  (c1, c2) <- liftIO $ do
    -- create a counter for the number of instantiations
    counter <- newIORef 0

    let r =
          funTo @IO newC1
            <: funTo @IO newC2
            <: funTo @IO (newSing counter)

    r' <- memoize @IO @Sing r
    c1 <- make @(IO C1) r'
    c2 <- make @(IO C2) r'
    pure (c1, c2)

  c1 === C1 (Sing 1)
  c2 === C2 (Sing 1)

test_automatic_memoize_all_for_with_registry = test "withRegistry automatically uses memoizeAll with RIO" $ do
  messagesRef <- liftIO $ newIORef []
  let registry =
        funTo @RIO App
          <: funTo @RIO newA
          <: funTo @RIO newB
          <: fun (newC messagesRef)

  --  just instantiate the app for its effects
  withRegistry @App registry $ \_ -> pure ()

  ms <- liftIO $ readIORef messagesRef

  annotate "if memoize works properly, then only one instantiation is invoked"
  ms === ["x"]

test_memoize_with_specialization = test "all the values on a specialization path are memoized independently" $ do
  D3 d1 d2 <- liftIO $ do
    -- create a counter for the number of instantiations
    counter <- newIORef 0

    let r =
          specialize @(IO D2) @(IO Specialized) (pure Specialized2) $
              funTo @IO D3
              <: funTo @IO D1
              <: funTo @IO D2
              <: funTo @IO (newCounter counter)
              <: valTo @IO Specialized1

    r' <- memoizeAll @IO r
    make @(IO D3) r'

  d1 === D1 (Counter 1 Specialized1)
  d2 === D2 (Counter 2 Specialized2)

-- * HELPERS

newtype C1 = C1 Sing deriving (Eq, Show)

newC1 :: Sing -> IO C1
newC1 = pure . C1

newtype C2 = C2 Sing deriving (Eq, Show)

newC2 :: Sing -> IO C2
newC2 = pure . C2

newtype Sing = Sing Int deriving (Eq, Show)

newSing :: IORef Int -> IO Sing
newSing counter = do
  _ <- modifyIORef counter (+ 1)
  i <- readIORef counter
  pure (Sing i)

data C3 = C3 C1 C2 deriving (Eq, Show)

newtype A = A {doItA :: IO ()}

newtype B = B {doItB :: IO ()}

newtype C = C {doItC :: IO ()}

newA :: C -> A
newA c = A {doItA = doItC c}

newB :: C -> B
newB c = B {doItB = doItC c}

newC :: IORef [Text] -> RIO C
newC messagesRef = do
  let c = C {doItC = pure ()}
  liftIO $ modifyIORef messagesRef ("x" :)
  pure c

data App = App {a :: A, b :: B}


newtype D1 = D1 Counter deriving (Eq, Show)
newtype D2 = D2 Counter deriving (Eq, Show)
data Counter = Counter Int Specialized deriving (Eq, Show)

data Specialized = Specialized1 | Specialized2 deriving (Eq, Show)

newCounter :: IORef Int -> Specialized -> IO Counter
newCounter counter specialized = do
  _ <- modifyIORef counter (+ 1)
  i <- readIORef counter
  pure (Counter i specialized)

data D3 = D3 D1 D2 deriving (Eq, Show)
