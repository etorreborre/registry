{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Test.Data.Registry.Internal.DynamicSpec where

import           Data.Dynamic
import           Data.Text as T
import           Data.Registry.Internal.Dynamic
import           Protolude                      as P
import           Test.Tasty.Extensions
import           Type.Reflection                hiding (typeRep)

test_is_function = test "we can check if a Dynamic value is a function" $ do

  isFunction (toDyn (u :: Int -> Int))    === True
  isFunction (toDyn (u :: Int -> IO Int)) === True
  isFunction (toDyn (u :: Int))           === False
  isFunction (toDyn (u :: IO Int))        === False

test_collectInputTypes = test "we can collect the input types of a function" $ do

  collectInputTypes (toDyn (u :: Int)) === []
  collectInputTypes (toDyn (u :: Text -> Int)) === [dynType (u :: Text)]
  collectInputTypes (toDyn (u :: Int -> Text -> Int)) === [dynType (u :: Int), dynType (u :: Text)]
  collectInputTypes (toDyn (u :: Int -> Maybe Double -> Maybe Text)) === [dynType (u :: Int), dynType (u :: Maybe Double)]

test_outputType = test "we can get the output type of a function" $ do

  outputType (dynType (u :: Int)) === dynType (u :: Int)
  outputType (dynType (u :: Text -> Int)) === dynType (u :: Int)
  outputType (dynType (u :: Int -> Text -> Int)) === dynType (u :: Int)
  outputType (dynType (u :: Int -> Maybe Double -> Maybe Text)) === dynType (u :: Maybe Text)

test_applyFunction = test "we can apply a list of dynamic values to a dynamic function" $ do

  fromDynamic @Int (applyFunction (toDyn T.length) [toDyn ("hello" :: Text)]) === Just 5

  let add1 (i::Int) (j::Int) = show (i + j) :: Text
  fromDynamic @Text (applyFunction (toDyn add1) [toDyn (1 :: Int), toDyn (2 :: Int)]) === Just "3"

  -- an exception is raised when an input parameter is incorrect
  gotException $ fromDynamic @Int (applyFunction (toDyn T.length) [toDyn (1 :: Int)])

  -- an exception is raised when there are not enough inputs
  gotException $ fromDynamic @Text (applyFunction (toDyn add1) [])


u = undefined

dynType :: forall a . (Typeable a) => a -> SomeTypeRep
dynType = dynTypeRep . toDyn

----
tests = $(testGroupGenerator)
