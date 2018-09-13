{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.WarmupSpec where

import           Control.Monad.Catch
import           Prelude                (show)
import           Protolude
import           Test.Tasty.Extensions
import           Data.Registry.Warmup

test_runBoth1 =
  prop "all results are collected when running 2 start/stop tasks" $ do
    r1 <- forAll genResult
    r2 <- forAll genResult
    r  <- liftIO $ pure r1 `runBoth` pure r2
    messages r === messages r1 ++ messages r2

test_runBoth2 =
  prop "exception messages are also collected" $ do
    r  <- liftIO $ throwM (Error "boom1") `runBoth` throwM (Error "boom2")
    messages r === ["boom1", "boom2"]

-- * helpers
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

----
tests = $(testGroupGenerator)
