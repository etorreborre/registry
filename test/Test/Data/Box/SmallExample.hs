{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-
  This module tests the construction of some simple values
  using a registry
-}
module Test.Data.Box.SmallExample where

import           Data.Box.Make
import           Data.Text (splitOn)
import           Data.Typeable (Typeable)
import           Protolude
import           Test.Tasty.Extensions
import           Hedgehog (assert)

-- | Components of the application
newtype Logger = Logger {
  info :: Text -> IO ()
} deriving Typeable

newLogger :: Logger
newLogger = Logger print

newtype LinesCounter = LinesCounter {
  count :: Text -> Int
} deriving Typeable

newLinesCounter :: LinesCounter
newLinesCounter = LinesCounter $ \t -> length (splitOn "\n" t)

newtype S3 = S3 {
  store :: Text -> IO ()
} deriving Typeable

data S3Config = S3Config {
   bucket :: Text
,  key    :: Text
} deriving (Eq, Show, Typeable)

newS3 :: S3Config -> Logger -> S3
newS3 config logger = S3 $
  \t -> (logger & info) ("storing on S3 with config " <> show config) >>
        void (print t) -- send the text to s3

newtype Application = Application {
  run :: Text -> IO ()
} deriving Typeable

newApplication :: Logger -> LinesCounter -> S3 -> Application
newApplication logger counter s3 = Application $ \t -> do
  (logger & info) "count lines"
  let n = (counter & count) t

  (logger & info) "store the lines on s3"
  (s3 & store) ("counted " <> show n <> " lines")

-- | Create a registry for all constructors
registry =
     val (S3Config "bucket" "key")
  +: fun newS3
  +: fun newLogger
  +: fun newLinesCounter
  +: fun newApplication
  +: end

-- | To create the application you call `make` for the `Application` type
--   with the registry above
--   Since the registry contains all functions and values necessary to create the application
--   Everything will work fine
createApplication :: Application
createApplication = make @Application registry

test_create = test "create the application" $ do
  _ <- pure createApplication -- nothing should crash!
  assert True