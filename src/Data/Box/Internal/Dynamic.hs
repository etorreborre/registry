{-# LANGUAGE AllowAmbiguousTypes #-}
{-
  Utility functions to work with Dynamic values
-}
module Data.Box.Internal.Dynamic where

import           Data.Box.Internal.Reflection
import           Data.Dynamic
import           Data.Text
import           Prelude             as Prelude
import           Type.Reflection

-- | Return true if the type of this dynamic variable is a function
isFunction :: Dynamic -> Bool
isFunction d =
  case dynTypeRep d of
    SomeTypeRep (Fun _ _) -> True
    _                     -> False

-- | Apply a Dynamic function to a list of Dynamic values
applyFunction
  :: Dynamic    -- function
  -> [Dynamic]  -- inputs
  -> Dynamic    -- result
applyFunction f [] =
  Prelude.error
    $  "the function "
    ++ Prelude.show (dynTypeRep f)
    ++ " cannot be applied to an empty list of parameters"
applyFunction f [i     ] = dynApp f i
applyFunction f (i : is) = applyFunction (dynApp f i) is

-- | If Dynamic is a function collect all its input types
collectInputTypes :: Dynamic -> [SomeTypeRep]
collectInputTypes = go . dynTypeRep
 where
  go :: SomeTypeRep -> [SomeTypeRep]
  go (SomeTypeRep (Fun in1 out)) = SomeTypeRep in1 : go (SomeTypeRep out)
  go _                           = []

-- | If the input type is a function type return its output type
outputType :: SomeTypeRep -> SomeTypeRep
outputType (SomeTypeRep (Fun _ out)) = outputType (SomeTypeRep out)
outputType r                         = r

describeValue :: (Typeable a, Show a) => a -> Text
describeValue = showValue

describeFunction :: (Typeable a) => a -> Text
describeFunction = showFunction
