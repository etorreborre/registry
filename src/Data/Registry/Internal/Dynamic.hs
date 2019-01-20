{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
  Utility functions to work with 'Dynamic' values
-}
module Data.Registry.Internal.Dynamic where

import           Data.Dynamic
import           Data.Registry.Internal.Types
import           Data.Text
import           Protolude as P
import           Type.Reflection

-- | Apply a function to a list of 'Dynamic' values
applyFunction ::
     Function           -- ^ function
  -> [Value]            -- ^ inputs
  -> Either Text Value  -- ^ result
applyFunction function [] =
  if P.null (collectInputTypes function) then
    pure $ makeCreatedValue (funDyn function) (ValueDescription (_outputType . funDescription $ function) Nothing) mempty
  else
    Left $  "the function "
    <> show (dynTypeRep (funDyn function))
    <> " cannot be applied to an empty list of parameters"

applyFunction function values =
  do created <- applyFunctionDyn (funDyn function) (valueDyn <$> values)
     let description  = ValueDescription (_outputType . funDescription $ function) Nothing
     let dependencies = foldMap dependenciesOn values

     pure $ makeCreatedValue created description dependencies

-- | Apply a Dynamic function to a list of Dynamic values
applyFunctionDyn ::
     Dynamic             -- ^ function
  -> [Dynamic]           -- ^ inputs
  -> Either Text Dynamic -- ^ result
applyFunctionDyn f [] =
  Left $  "the function "
         <> show (dynTypeRep f)
         <> " cannot be applied to an empty list of parameters"
applyFunctionDyn f [i     ] = applyOneParam f i
applyFunctionDyn f (i : is) = do
  f' <- applyOneParam f i
  applyFunctionDyn f' is

-- | Apply just one dynamic parameter to a dynamic function
applyOneParam :: Dynamic -> Dynamic -> Either Text Dynamic
applyOneParam f i =
  maybe (Left $ "failed to apply " <> show i <> " to : " <> show f) Right (dynApply f i)

-- | If Dynamic is a function collect all its input types
collectInputTypes :: Function -> [SomeTypeRep]
collectInputTypes = go . funDynTypeRep
 where
  go :: SomeTypeRep -> [SomeTypeRep]
  go (SomeTypeRep (Fun in1 out)) = SomeTypeRep in1 : go (SomeTypeRep out)
  go _                           = []

-- | If the input type is a function type return its output type
outputType :: SomeTypeRep -> SomeTypeRep
outputType (SomeTypeRep (Fun _ out)) = outputType (SomeTypeRep out)
outputType r                         = r
