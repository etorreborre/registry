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

import           Data.List                         hiding (unlines)
import           Data.Registry.Internal.Dynamic
import           Data.Registry.Internal.Reflection (showSingleType)
import           Data.Registry.Internal.Registry
import           Data.Registry.Internal.Stack
import           Data.Registry.Internal.Types
import qualified Data.Text                         as T
import           Protolude                         as P hiding (Constructor)
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
  let foundValue = findValue targetType context specializations values

  case foundValue of
    Nothing ->
      -- if not, is there a way to build such value?
      case findConstructor targetType functions of
        Nothing -> lift $ Left $
             "When trying to create the following values\n\n          "
          <> T.intercalate "\nrequiring " (showContextTargets context)
          <> "\n\nNo constructor was found for " <> showSingleType targetType

        Just function -> do
          let inputTypes = collectInputTypes function
          inputs <- makeInputs function inputTypes context functions specializations modifiers

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

-- | Show the target type and possibly the constructor function requiring it
--   for every target type in the context
showContextTargets :: Context -> [Text]
showContextTargets (Context context) =
  fmap (\(t, f) ->
    case f of
       Nothing       -> show t
       Just function -> show t <> "\t\t\t(required for the constructor " <> show function <> ")")
  (reverse context)

-- | Make the input values of a given function
--   When a value has been made it is placed on top of the
--   existing registry so that it is memoized if needed in
--   subsequent calls
makeInputs ::
     Function
  -> [SomeTypeRep]   -- ^ input types to build
  -> Context         -- ^ current context of types being built
  -> Functions       -- ^ available functions to build values
  -> Specializations -- ^ list of values to use when in a specific context
  -> Modifiers       -- ^ modifiers to apply before storing made values
  -> Stack [Value]   -- list of made values
makeInputs _ [] _ _ _ _ = pure []

makeInputs function (i : ins) c@(Context context) functions specializations modifiers =
  if i `elem` (contextTypes c)
    then
      lift $ Left
      $  toS
      $  unlines
      $  ["cycle detected! The current types being built are "]
      <> (show <$> context)
      <> ["But we are trying to build again " <> show i]
    else do
      madeInput <- makeUntyped i (Context ((i, Just (funDynTypeRep function)) : context)) functions specializations modifiers
      case madeInput of
        Nothing ->
          -- if one input cannot be made, iterate with the rest for better reporting
          -- of what could be eventually made
          makeInputs function ins (Context context) functions specializations modifiers

        Just v ->
          (v :) <$> makeInputs function ins (Context context) functions specializations modifiers
