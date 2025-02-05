{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Tutorial.DatabaseLoggerExample where

import Data.Registry
import Test.Tutorial.DatabaseLogger

prodRegistry =
     fun newDatabase
  <: fun newLogger

prodDatabase = make @Database prodRegistry

-- Don't log anything for those tests
testRegistry1 =
     fun noLogger
  <: prodRegistry

testDatabase1 = make @Database testRegistry1

-- Only log some messages for those tests
testRegistry2 =
  tweak @Logger (limitSeverity Info) prodRegistry

testDatabase2 = make @Database testRegistry2

-- Only log some messages for those tests
-- Specify the severity as a separate value
testRegistry3 =
     fun newLimitedLogger
  <: val Info
  <: prodRegistry

testDatabase3 = make @Database testRegistry3
