{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-
  Untyped implementation of the functionalities in
    Data.Registry.Make
-}
module Data.Registry.Internal.Make where

import           Data.Dynamic
import           Data.List                         hiding (unlines)
import           Data.Registry.Internal.Dynamic
import           Data.Registry.Internal.Registry
import           Data.Text                         as T (unlines)
import qualified Prelude                           (error)
import           Protolude                         as P hiding (Constructor)
import           Type.Reflection

-- * WARNING: HIGHLY UNTYPED IMPLEMENTATION !

-- | Make a value from a desired output type represented by SomeTypeRep
--   and a list of possible constructors
--   A context is passed in the form of a stack of the types we are trying to build so far
makeUntyped
  :: SomeTypeRep
  -> Context
  -> Functions
  -> Specializations
  -> Modifiers
  -> State Values (Maybe Dynamic)
makeUntyped targetType context functions specializations modifiers = do
  values <- get

  -- is there already a value with the desired type?
  case findValue targetType context specializations values of
    Nothing ->
      -- if not, is there a way to build such value?
      case findConstructor targetType functions of
        Nothing -> pure Nothing

        Just c  -> do
          let inputTypes = collectInputTypes c
          inputs <- makeInputs inputTypes context functions specializations modifiers


          if length inputs /= length inputTypes
            then
              let madeInputTypes = fmap dynTypeRep inputs
                  missingInputTypes = inputTypes \\ madeInputTypes
              in
                Prelude.error
                $  toS
                $  unlines
                $  ["could not make all the inputs for ", show c, ". Only "]
                <> (show <$> inputs)
                <> ["could be made. Missing"]
                <> (fmap show missingInputTypes)
            else do
              let v = applyFunction c inputs
              modified <- storeValue modifiers v
              pure (Just modified)


    Just v -> do
      modified <- storeValue modifiers v
      pure (Just modified)

-- | Make the input values of a given function
--   When a value has been made it is placed on top of the
--   existing registry so that it is memoized if needed in
--   subsequent calls
makeInputs
  :: [SomeTypeRep]
  -> Context
  -> Functions
  -> Specializations
  -> Modifiers
  -> State Values [Dynamic] -- list of made values
makeInputs [] _ _ _ _ = pure []

makeInputs (i : ins) (Context context) functions specializations modifiers =
  if i `elem` context
    then
      Prelude.error
      $  toS
      $  unlines
      $  ["cycle detected! The current types being built are "]
      <> (show <$> context)
      <> ["But we are trying to build again " <> show i]
    else do
      madeInput <- makeUntyped i (Context (i : context)) functions specializations modifiers
      case madeInput of
        Nothing ->
          -- if one input cannot be made, iterate with the rest for better reporting
          -- of what could be eventually made
          makeInputs ins (Context context) functions specializations modifiers

        Just v ->
          (v :) <$> makeInputs ins (Context context) functions specializations modifiers
