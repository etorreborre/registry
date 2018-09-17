{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
  List of types used inside the Registry
-}
module Data.Registry.Internal.Types where

import           Data.Dynamic
import           Data.Registry.Internal.Reflection
import           Data.Text                         as T
import           Prelude                           (show)
import           Protolude                         hiding (show)
import           Type.Reflection

-- | A Function is the Dynamic representation of a Haskell value + its description
--   It is either provided by the user of the Registry or created as part of the
--   resolution algorithm
data Value =
    CreatedValue  Dynamic ValueDescription
  | ProvidedValue Dynamic ValueDescription
  deriving (Show)

-- | Description of a value. It might just have
--   a description for its type when it is a value
--   created by the resolution algorithm
data ValueDescription = ValueDescription {
    _valueType  :: Text
  , _valueValue :: Maybe Text
  } deriving (Eq, Show)

-- | Describe a value with its type and actual content
describeValue :: (Typeable a, Show a) => a -> ValueDescription
describeValue a = ValueDescription (showFullValueType a) (Just . toS $ show a)

-- | Describe a value with only its type
describeTypeableValue :: (Typeable a) => a -> ValueDescription
describeTypeableValue a = ValueDescription (showFullValueType a) Nothing

-- | Show a Value from the Registry
showValue :: Value -> Text
showValue = valDescriptionToText . valDescription

-- | Create a Value from a Haskell value, with its Show description
createValue :: (Show a, Typeable a) => a -> Value
createValue a = ProvidedValue (toDyn a) (describeValue a)

-- | Create a Value from a Haskell value, with only its Typeable description
createTypeableValue :: Typeable a => a -> Value
createTypeableValue a = ProvidedValue (toDyn a) (describeTypeableValue a)

-- | Create a Value from a Dynamic value and some description
createDynValue :: Dynamic -> Text -> Value
createDynValue dyn desc = ProvidedValue dyn (ValueDescription desc Nothing)

-- | Type representation of a Value
valueDynTypeRep :: Value -> SomeTypeRep
valueDynTypeRep (CreatedValue  d _) = dynTypeRep d
valueDynTypeRep (ProvidedValue d _) = dynTypeRep d

-- | Dynamic representation of a Value
valueDyn :: Value -> Dynamic
valueDyn (CreatedValue  d _) = d
valueDyn (ProvidedValue d _) = d

-- | The description for a Value
valDescription :: Value -> ValueDescription
valDescription (CreatedValue  _ d) = d
valDescription (ProvidedValue _ d) = d

-- | A ValueDescription as Text. If the actual content of the Value
--   is provided display the type first then the content
valDescriptionToText :: ValueDescription -> Text
valDescriptionToText (ValueDescription t Nothing) = t
valDescriptionToText (ValueDescription t (Just v)) = t <> ": " <> v

-- | A Function is the Dynamic representation of a Haskell function + its description
data Function = Function Dynamic FunctionDescription deriving (Show)

-- | Create a Function value from a Haskell function
createFunction :: (Typeable a) => a -> Function
createFunction a =
  let dynType = toDyn a
  in  Function dynType (describeFunction a)

-- | Description of a function with input types and output type
data FunctionDescription = FunctionDescription {
    _inputTypes :: [Text]
  , _outputType :: Text
  } deriving (Eq, Show)

-- | Describe a function (which doesn't have a Show instance)
--   that can be put in the Registry
describeFunction :: Typeable a => a -> FunctionDescription
describeFunction = uncurry FunctionDescription . showFullFunctionType

-- | Show a Function as Text using its Description
showFunction :: Function -> Text
showFunction = funDescriptionToText . funDescription

-- | The Description of a Function
funDescription :: Function -> FunctionDescription
funDescription (Function _ t) = t

-- | Dynamic representation of a Function
funDyn :: Function -> Dynamic
funDyn (Function d _) = d

-- | Type representation of a Function
funDynTypeRep :: Function -> SomeTypeRep
funDynTypeRep = dynTypeRep . funDyn

-- | A FunctionDescription as Text
funDescriptionToText :: FunctionDescription -> Text
funDescriptionToText (FunctionDescription ins out) = T.intercalate " -> " (ins <> [out])

-- | Return True if a Function has some input values
hasParameters :: Function -> Bool
hasParameters = isFunction . funDynTypeRep

-- | A Typed value can be added to a Registry
--   It is either a value, having both Show and Typeable information
--   or a function having just Typeable information
data Typed a =
    TypedValue Value
  | TypedFunction Function

-- The list of functions available for constructing other values
newtype Functions = Functions [Function] deriving (Show, Semigroup, Monoid)

-- | Add one more Function to the list of Functions
addFunction :: Function -> Functions -> Functions
addFunction f (Functions fs) = Functions (f : fs)

-- | List of values available for constructing other values
newtype Values = Values [Value] deriving (Show, Semigroup, Monoid)

-- | Add one more Value to the list of Values
addValue :: Value -> Values -> Values
addValue v (Values vs) = Values (v : vs)

-- | The types of values being currently built
newtype Context = Context { _context :: [SomeTypeRep] } deriving (Show, Semigroup, Monoid)

-- | Specification of values which become available for
--   construction when a corresponding type comes in context
newtype Specializations = Specializations [(SomeTypeRep, Value)] deriving (Show, Semigroup, Monoid)

-- | List of functions modifying some values right after they have been
--   built. This enables "tweaking" the creation process with slightly
--   different results. Here SomeTypeRep is the target value type 'a' and
newtype Modifiers = Modifiers [(SomeTypeRep, Function)] deriving (Show, Semigroup, Monoid)
