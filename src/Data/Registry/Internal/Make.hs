{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{- |
  Untyped implementation of the functionalities in
    'Data.Registry.Make'
-}
module Data.Registry.Internal.Make where

import           Data.List                       hiding (unlines)
import           Data.Registry.Internal.Dynamic
import           Data.Registry.Internal.Registry
import           Data.Registry.Internal.Types
import           Data.Registry.Internal.Stack
import           Data.Text                       as T (unlines)
import           Protolude                       as P hiding (Constructor)
import           Type.Reflection

-- * WARNING: HIGHLY UNTYPED IMPLEMENTATION !

-- | Make a value from a desired output type represented by SomeTypeRep
--   and a list of possible constructors
--   A 'Context' is passed in the form of a stack of the types we are trying to build so far
--  Functions is the list of all the constructors in the Registry
--  Specializations is a list of specific values to use in a given context, overriding the normal search
--  Modifiers is a list of functions to apply right before a value is stored in the Registry
makeUntyped ::
     SomeTypeRep
  -> Context
  -> Functions
  -> Specializations
  -> Modifiers
  -> Stack (Maybe Value)
makeUntyped targetType context functions specializations modifiers = do
  values <- getValues

  -- is there already a value with the desired type?
  case findValue targetType context specializations values of
    Nothing ->
      -- if not, is there a way to build such value?
      case findConstructor targetType functions of
        Nothing -> lift $ Left ("cannot find a constructor for " <> show targetType)

        Just function -> do
          let inputTypes = collectInputTypes function
          inputs <- makeInputs inputTypes context functions specializations modifiers

          if length inputs /= length inputTypes
            then
              -- report an error if we cannot make enough input parameters to apply the function
              let madeInputTypes = fmap valueDynTypeRep inputs
                  missingInputTypes = inputTypes \\ madeInputTypes
              in
                lift $ Left $
                  unlines
                $  ["could not make all the inputs for ", show (funDescription function), ". Only "]
                <> (show <$> inputs)
                <> ["could be made. Missing"]
                <> fmap show missingInputTypes
            else do
              -- else apply the function and store the output value in the registry
              value <- lift $ applyFunction function inputs
              modified <- storeValue modifiers value

              functionApplied modified inputs
              pure (Just modified)


    Just v -> do
      modified <- storeValue modifiers v
      pure (Just modified)

-- | Make the input values of a given function
--   When a value has been made it is placed on top of the
--   existing registry so that it is memoized if needed in
--   subsequent calls
makeInputs ::
     [SomeTypeRep]   -- ^ input types to build
  -> Context         -- ^ current context of types being built
  -> Functions       -- ^ available functions to build values
  -> Specializations -- ^ list of values to use when in a specific context
  -> Modifiers       -- ^ modifiers to apply before storing made values
  -> Stack [Value]   -- list of made values
makeInputs [] _ _ _ _ = pure []

makeInputs (i : ins) (Context context) functions specializations modifiers =
  if i `elem` context
    then
      lift $ Left
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
