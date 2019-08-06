{-# LANGUAGE DataKinds        #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Make.MemoizeSpec where

import           Data.Registry
import           Data.IORef
import           Protolude hiding (C1)
import           Test.Tasty.Extensions
import           System.IO.Memoize

-- | Creation of values with memoization
test_memoize = test "effectful values can be memoized with System.IO.Memoize" $ do
  (c1, c2) <- liftIO $
    do -- create a counter for the number of instantiations
       counter <- newIORef 0

       newSingOnce <- once (newSing counter)
       let r =    funTo @IO newC1
               +: funTo @IO newC2
               +: funTo @IO newSingOnce
               +: end
       c1 <- make @(IO C1) r
       c2 <- make @(IO C2) r
       pure (c1, c2)

  c1 === C1 (Sing 1)
  c2 === C2 (Sing 1)

test_memoize_proper = test "effectful values can memoized" $ do
  (c1, c2) <- liftIO $
    do -- create a counter for the number of instantiations
       counter <- newIORef 0

       let r =    funTo @IO newC1
               +: funTo @IO newC2
               +: funTo @IO (newSing counter)
               +: end
       r' <- memoize @IO @Sing r
       c1 <- make @(IO C1) r'
       c2 <- make @(IO C2) r'
       pure (c1, c2)

  c1 === C1 (Sing 1)
  c2 === C2 (Sing 1)

newtype C1 = C1 Sing deriving (Eq, Show)
newC1 :: Sing -> IO C1
newC1 = pure . C1

newtype C2 = C2 Sing deriving (Eq, Show)
newC2 :: Sing -> IO C2
newC2 = pure . C2

newtype Sing = Sing Int deriving (Eq, Show)
newSing :: IORef Int -> IO Sing
newSing counter = do
  _ <- modifyIORef counter (+1)
  i <- readIORef counter
  pure (Sing i)

---

test_automatic_memoizeAll_for_with_registry =
  test "withRegistry automatically uses memoizeAll with RIO" $ do
    messagesRef <- liftIO $ newIORef []
    let registry =
            funTo @RIO App
         +: funTo @RIO newA
         +: funTo @RIO newB
         +: fun   (newC messagesRef)
         +: end

    --  just instantiate the app for its effects
    withRegistry @App registry $ \_ _ -> pure ()

    ms <- liftIO $ readIORef messagesRef

    annotate "if memoize works properly, then only one warmup is invoked"
    ms === ["x"]

newtype A = A { doItA :: IO () }
newtype B = B { doItB :: IO () }
newtype C = C { doItC :: IO () }

newA :: C -> A
newA c = A { doItA = doItC c }

newB :: C -> B
newB c = B { doItB = doItC c }

newC :: IORef [Text] -> RIO C
newC messagesRef = do
  let c = C { doItC = pure () }
  warmupWith (createWarmup (modifyIORef messagesRef ("x":) $> Ok ["good"]))
  pure c

data App = App { a :: A, b :: B }
