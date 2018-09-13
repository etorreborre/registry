{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Registry.Internal.Types where

import           Data.Dynamic
import           Data.Registry.Internal.Reflection
import           Data.Text                         as T
import           Prelude                           (show)
import           Protolude                         hiding (show)
import           Type.Reflection

-- List of types currently being built
newtype Context = Context { _context :: [SomeTypeRep] } deriving (Show, Semigroup, Monoid)

-- List of functions available for constructing other values
newtype Functions = Functions [Function] deriving (Show, Semigroup, Monoid)
data Function = Function Dynamic FunctionDescription deriving (Show)

createFunction :: (Typeable a) => a -> Function
createFunction a =
  let dynType = toDyn a
  in  Function dynType (describeFunction a)

data FunctionDescription = FunctionDescription {
    _inputTypes :: [Text]
  , _outputType :: Text
  } deriving (Eq, Show)

data Typed a =
    TypedValue Value
  | TypedFunction Function

-- | Show a function (which doesn't have a Show instance)
describeFunction :: Typeable a => a -> FunctionDescription
describeFunction = uncurry FunctionDescription . showFullFunctionType

showFunction :: Function -> Text
showFunction = funDescriptionToText . funDescription

funDescription :: Function -> FunctionDescription
funDescription (Function _ t) = t

funDyn :: Function -> Dynamic
funDyn (Function d _) = d

funDynTypeRep :: Function -> SomeTypeRep
funDynTypeRep = dynTypeRep . funDyn

addFunction :: Function -> Functions -> Functions
addFunction f (Functions fs) = Functions (f : fs)

hasParameters :: Function -> Bool
hasParameters = isFunction . funDynTypeRep

-- List of values available for constructing other values
newtype Values = Values [Value] deriving (Show, Semigroup, Monoid)

addValue :: Value -> Values -> Values
addValue v (Values vs) = Values (v : vs)

-- Specification of values which become available for
-- construction when a corresponding type comes in context
newtype Specializations = Specializations [(SomeTypeRep, Value)] deriving (Show, Semigroup, Monoid)

-- List of functions modifying some values right after they have been
-- built. This enables "tweaking" the creation process with slightly
-- different results. Here SomeTypeRep is the target value type 'a' and
newtype Modifiers = Modifiers [(SomeTypeRep, Function)] deriving (Show, Semigroup, Monoid)

data Value =
    CreatedValue  Dynamic ValueDescription
  | ProvidedValue Dynamic ValueDescription
  deriving (Show)

data ValueDescription = ValueDescription {
    _valueType  :: Text
  , _valueValue :: Maybe Text
  } deriving (Eq, Show)

-- | Show a value with its type first
describeValue :: (Typeable a, Show a) => a -> ValueDescription
describeValue a = ValueDescription (showFullValueType a) (Just . toS $ show a)

-- | Show a value with only its type
describeTypeableValue :: (Typeable a) => a -> ValueDescription
describeTypeableValue a = ValueDescription (showFullValueType a) Nothing

showValue :: Value -> Text
showValue = valDescriptionToText . valDescription

createValue :: (Show a, Typeable a) => a -> Value
createValue a = ProvidedValue (toDyn a) (describeValue a)

createTypeableValue :: Typeable a => a -> Value
createTypeableValue a = ProvidedValue (toDyn a) (describeTypeableValue a)

createDynValue :: Dynamic -> Text -> Value
createDynValue dyn desc = ProvidedValue dyn (ValueDescription desc Nothing)

valueDynTypeRep :: Value -> SomeTypeRep
valueDynTypeRep (CreatedValue  d _) = dynTypeRep d
valueDynTypeRep (ProvidedValue d _) = dynTypeRep d

valueDyn :: Value -> Dynamic
valueDyn (CreatedValue  d _) = d
valueDyn (ProvidedValue d _) = d

valDescription :: Value -> ValueDescription
valDescription (CreatedValue  _ d) = d
valDescription (ProvidedValue _ d) = d

valDescriptionToText :: ValueDescription -> Text
valDescriptionToText (ValueDescription t Nothing) = t
valDescriptionToText (ValueDescription t (Just v)) = t <> ": " <> v

funDescriptionToText :: FunctionDescription -> Text
funDescriptionToText (FunctionDescription ins out) = T.intercalate " -> " (ins <> [out])
