{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.WarmupSpec where

import           Control.Monad.Catch
import           Data.IORef
import           Data.Registry
import           Prelude               (show)
import           Protolude
import           Test.Tasty.Extensions

test_runBoth1 =
  prop "all results are collected when running 2 warmup tasks" $ do
    r1 <- forAll genResult
    r2 <- forAll genResult
    r  <- liftIO $ pure r1 `runBoth` pure r2
    messages r === messages r1 ++ messages r2

test_runBoth2 =
  prop "exception messages are also collected" $ do
    r  <- liftIO $ throwM (Error "boom1") `runBoth` throwM (Error "boom2")
    messages r === ["boom1", "boom2"]

test_run_side_effects_once =
  test "a component having a warmup must be memoized" $ do
    messagesRef <- liftIO $ newIORef []
    registry <-
       liftIO $ memoizeAll @RIO $
             funTo @RIO App
          +: funTo @RIO newA
          +: funTo @RIO newB
          +: fun   (newC messagesRef)
          +: end

    void $ withRIO (makeUnsafe @(RIO App) registry) $ const (pure ())

    ms <- liftIO $ readIORef messagesRef
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

-- * HELPERS
newtype Error = Error Text

instance Show Error where
  show (Error t) = toS t

instance Exception Error

genResult :: Gen Result
genResult =
  choice [genEmpty, genFailed, genOk]

genEmpty  = pure Empty
genFailed = failed <$> element simpsons
genOk     = ok     <$> element colours
