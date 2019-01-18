{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
  List of types used inside the Registry
-}
module Data.Registry.Internal.Types where

import           Data.Dynamic
import           Data.Hashable
import           Data.List                         (intersect)
import           Data.List.NonEmpty
import           Data.Registry.Internal.Reflection
import           Data.Text                         as T
import           Prelude                           (show)
import           Protolude                         as P hiding (show)
import qualified Protolude                         as P
import           Type.Reflection

-- | A 'Value' is the 'Dynamic' representation of a Haskell value + its description
--   It is either provided by the user of the Registry or created as part of the
--   resolution algorithm
--   If a `Context` is present for a a created value this means that this value
--   has been written as the result of a specialization. The first type of the
--   list of types in the context is the types under which the specialization must
--   apply and the other types are "parents" of the current value in the value
--   graph
data Value =
    CreatedValue  Dynamic ValueDescription (Maybe Context) Dependencies
  | ProvidedValue Dynamic ValueDescription
  deriving (Show)

-- | This registers the specific context in which a valu
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

-- | Show a Value from the 'Registry'
showValue :: Value -> Text
showValue = valDescriptionToText . valDescription

-- | Create a Value from a Haskell value, using its Show instance for its description
createValue :: (Typeable a, Show a) => a -> Value
createValue a = makeProvidedValue (toDyn a) (describeValue a)

-- | Make a ProvidedValue
makeProvidedValue :: Dynamic -> ValueDescription -> Value
makeProvidedValue = ProvidedValue

-- | make a CreatedValue in no particular context
makeCreatedValue :: Dynamic -> ValueDescription -> Dependencies -> Value
makeCreatedValue d desc = CreatedValue d desc Nothing

-- | Create a Value from a Haskell value, with only its 'Typeable' description
createTypeableValue :: Typeable a => a -> Value
createTypeableValue a = ProvidedValue (toDyn a) (describeTypeableValue a)

-- | Create a Value from a 'Dynamic' value and some description
createDynValue :: Dynamic -> Text -> Value
createDynValue dyn desc = ProvidedValue dyn (ValueDescription desc Nothing)

-- | Type representation of a 'Value'
valueDynTypeRep :: Value -> SomeTypeRep
valueDynTypeRep = dynTypeRep . valueDyn

-- | Dynamic representation of a 'Value'
valueDyn :: Value -> Dynamic
valueDyn (CreatedValue  d _ _ _) = d
valueDyn (ProvidedValue d _)     = d

-- | The description for a 'Value'
valDescription :: Value -> ValueDescription
valDescription (CreatedValue  _ d _ _ ) = d
valDescription (ProvidedValue _ d)      = d

-- | The dependencies for a 'Value'
valDependencies :: Value -> Dependencies
valDependencies (CreatedValue  _ _ _ ds) = ds
valDependencies (ProvidedValue _ _)      = mempty

-- | A ValueDescription as 'Text'. If the actual content of the 'Value'
--   is provided display the type first then the content
valDescriptionToText :: ValueDescription -> Text
valDescriptionToText (ValueDescription t Nothing)  = t
valDescriptionToText (ValueDescription t (Just v)) = t <> ": " <> v

-- | Return the creation context for a given value when it was created
--   as the result of a "specialization"
specializationContext :: Value -> Maybe Context
specializationContext (CreatedValue _ _ context _) = context
specializationContext _                            = Nothing

-- | Return True if a type is part of the specialization context of a Value
isInSpecializationContext :: SomeTypeRep -> Value -> Bool
isInSpecializationContext target value =
  case specializationContext value of
    Just (Context cs) -> target `elem` cs
    Nothing           -> False

hasSpecializedInputsInThisContext :: Specializations -> Value -> Bool
hasSpecializedInputsInThisContext (Specializations ss) v =
  let Dependencies dependencies = valDependencies v
      targetTypes = specializationTargetType <$> ss
  in  not . P.null $ targetTypes `intersect` dependencies

-- | A Function is the 'Dynamic' representation of a Haskell function + its description
data Function = Function Dynamic FunctionDescription deriving (Show)

-- | Create a 'Function' value from a Haskell function
createFunction :: (Typeable a) => a -> Function
createFunction a =
  let dynType = toDyn a
  in  Function dynType (describeFunction a)

-- | Description of a 'Function' with input types and output type
data FunctionDescription = FunctionDescription {
    _inputTypes :: [Text]
  , _outputType :: Text
  } deriving (Eq, Show)

-- | Describe a 'Function' (which doesn't have a 'Show' instance)
--   that can be put in the 'Registry'
describeFunction :: Typeable a => a -> FunctionDescription
describeFunction = uncurry FunctionDescription . showFullFunctionType

-- | Show a Function as 'Text' using its Description
showFunction :: Function -> Text
showFunction = funDescriptionToText . funDescription

-- | The Description of a 'Function'
funDescription :: Function -> FunctionDescription
funDescription (Function _ t) = t

-- | Dynamic representation of a 'Function'
funDyn :: Function -> Dynamic
funDyn (Function d _) = d

-- | Type representation of a 'Function'
funDynTypeRep :: Function -> SomeTypeRep
funDynTypeRep = dynTypeRep . funDyn

-- | A 'FunctionDescription' as 'Text'
funDescriptionToText :: FunctionDescription -> Text
funDescriptionToText (FunctionDescription ins out) = T.intercalate " -> " (ins <> [out])

-- | Return True if a 'Function' has some input parameters
hasParameters :: Function -> Bool
hasParameters = isFunction . funDynTypeRep

-- | A Typed value or function can be added to a 'Registry'
--   It is either a value, having both 'Show' and 'Typeable' information
--   or a function having just 'Typeable' information
data Typed a =
    TypedValue Value
  | TypedFunction Function

-- | This is a list of functions (or "constructors") available for constructing values
newtype Functions = Functions [Function] deriving (Show, Semigroup, Monoid)

-- | Display a list of constructors
describeFunctions :: Functions -> Text
describeFunctions (Functions fs) =
  if P.null fs then
    ""
  else
    unlines (funDescriptionToText . funDescription <$> fs)

-- | Add one more Function to the list of Functions
addFunction :: Function -> Functions -> Functions
addFunction f (Functions fs) = Functions (f : fs)

-- | List of values available which can be used as parameters to
--   constructors for building other values
newtype Values = Values [Value] deriving (Show, Semigroup, Monoid)

-- | Display a list of values
describeValues :: Values -> Text
describeValues (Values vs) =
  if P.null vs then
    ""
  else
    unlines (valDescriptionToText . valDescription <$> vs)

-- | Add one more Value to the list of Values
addValue :: Value -> Values -> Values
addValue v (Values vs) = Values (v : vs)

-- | The types of values that we are trying to build at a given moment
--   of the resolution algorithm
newtype Context = Context {
  _context :: [SomeTypeRep]
} deriving (Eq, Hashable, Show, Semigroup, Monoid)

instance Ord Context where
  Context cs1 <= Context cs2 = P.all (`elem` cs2)  cs1

-- | The types of values that a value depends on
newtype Dependencies = Dependencies { _dependencies :: [SomeTypeRep] } deriving (Eq, Show, Semigroup, Monoid)

instance Ord Dependencies where
  Dependencies cs1 <= Dependencies cs2 = P.all (`elem` cs2)  cs1

-- | Specification of values which become available for
--   construction when a corresponding type comes in context
newtype Specializations = Specializations [Specialization] deriving (Show, Semigroup, Monoid)

data Specialization = Specialization {
  _specializationPath  :: NonEmpty SomeTypeRep
, _specializationValue :: Value
} deriving (Show)

specializationTargetType :: Specialization -> SomeTypeRep
specializationTargetType = valueDynTypeRep . _specializationValue

isTargetPath :: [SomeTypeRep] -> Specialization -> Bool
isTargetPath cs s = P.all (`elem` cs) (_specializationPath s)

-- | Display a list of specializations for the Registry, just showing the
--   context (a type) in which a value must be selected
describeSpecializations :: Specializations -> Text
describeSpecializations (Specializations ss) =
  if P.null ss then
    ""
  else
    "specializations\n" <> unlines (P.show <$> ss)

-- | List of functions modifying some values right after they have been
--   built. This enables "tweaking" the creation process with slightly
--   different results. Here SomeTypeRep is the target value type 'a' and
newtype Modifiers = Modifiers [(SomeTypeRep, Function)] deriving (Show, Semigroup, Monoid)

-- | Display a list of modifiers for the Registry, just showing the
--   type of the modified value
describeModifiers :: Modifiers -> Text
describeModifiers (Modifiers ms) =
  if P.null ms then
    ""
  else
    "modifiers for types\n" <> unlines (P.show . fst <$> ms)
