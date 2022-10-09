{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--  List of types used inside the Registry
module Data.Registry.Internal.Types where

import Data.Dynamic
import Data.Hashable
import Data.List (elemIndex, intersect)
import Data.List.NonEmpty
import Data.List.NonEmpty as NonEmpty (head, last)
import Data.Registry.Internal.Reflection
import qualified Data.Text as T hiding (last)
import Protolude as P hiding (show)
import qualified Protolude as P
import Type.Reflection
import Prelude (show)

-- | A 'Value' is the 'Dynamic' representation of a Haskell value + its description
--   It is either provided by the user of the Registry or created as part of the
--   resolution algorithm.
--
--   A value can simply be provided by the user of the registry or created as the
--   result of function application
--
--   Dependencies is the transitive list of all the values used to create a CreatedValue
--
--   The optional SpecializationContext is used for values created as the result of a specialization
--   It stores the context of creation (the list of types we are currently trying to build) and
--   the desired specialization (which must be a subtype of the context)
data Value
  = CreatedValue Dynamic ValueDescription (Maybe SpecializationContext) Dependencies
  | ProvidedValue Dynamic ValueDescription
  deriving (Show)

instance Eq Value where
  CreatedValue _ vd1 sc1 ds1 == CreatedValue _ vd2 sc2 ds2 =
    (vd1, sc1, ds1) == (vd2, sc2, ds2)
  ProvidedValue _ vd1 == ProvidedValue _ vd2 =
    vd1 == vd2
  _ == _ = False

instance Hashable Value where
  hash value = hash (valDescription value)
  hashWithSalt n value = hashWithSalt n (valDescription value)

-- | Description of a value. It might just have
--   a description for its type when it is a value
--   created by the resolution algorithm
data ValueDescription = ValueDescription
  { _valueType :: Text,
    _valueValue :: Maybe Text
  }
  deriving (Eq, Show)

instance Hashable ValueDescription where
  hash (ValueDescription d v) = hash (d, v)
  hashWithSalt n (ValueDescription d v) = hashWithSalt n (d, v)

-- | Describe a value with its type and actual content
describeValue :: (Typeable a, Show a) => a -> ValueDescription
describeValue a = ValueDescription (showFullValueType a) (Just . toS $ show a)

-- | Describe a value with only its type
describeTypeableValue :: (Typeable a) => a -> ValueDescription
describeTypeableValue a = ValueDescription (showFullValueType a) Nothing

-- | Show a Value from the Registry
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
valueDyn (CreatedValue d _ _ _) = d
valueDyn (ProvidedValue d _) = d

-- | The description for a 'Value'
valDescription :: Value -> ValueDescription
valDescription (CreatedValue _ d _ _) = d
valDescription (ProvidedValue _ d) = d

-- | The dependencies for a 'Value'
valueDependencies :: Value -> Dependencies
valueDependencies (CreatedValue _ _ _ ds) = ds
valueDependencies (ProvidedValue _ _) = mempty

-- | A ValueDescription as 'Text'. If the actual content of the 'Value'
--   is provided display the type first then the content
valDescriptionToText :: ValueDescription -> Text
valDescriptionToText (ValueDescription t Nothing) = t
valDescriptionToText (ValueDescription t (Just v)) = t <> ": " <> v

--  | Return the context + specialization used when specializing a value
valueSpecializationContext :: Value -> Maybe SpecializationContext
valueSpecializationContext (CreatedValue _ _ sc _) = sc
valueSpecializationContext _ = Nothing

--  | Return the context used when specializing a value
valueContext :: Value -> Maybe Context
valueContext (CreatedValue _ _ sc _) = scContext <$> sc
valueContext _ = Nothing

-- | Return the specialization used when specializing a value
valueSpecialization :: Value -> Maybe Specialization
valueSpecialization (CreatedValue _ _ sc _) = scSpecialization <$> sc
valueSpecialization _ = Nothing

-- | Return True if a type is part of the specialization context of a Value
isInSpecializationContext :: SomeTypeRep -> Value -> Bool
isInSpecializationContext target value =
  case valueContext value of
    Just context -> target `elem` contextTypes context
    Nothing -> False

-- | Return True if a value has transitives dependencies which are
--   specialized values
hasSpecializedDependencies :: Specializations -> Value -> Bool
hasSpecializedDependencies (Specializations ss) v =
  let DependenciesTypes ds = dependenciesTypes $ valueDependencies v
      targetTypes = specializationTargetType <$> ss
   in not . P.null $ targetTypes `intersect` ds

-- | A Function is the 'Dynamic' representation of a Haskell function + its description
data Function = Function Dynamic FunctionDescription deriving (Show)

-- | Create a 'Function' value from a Haskell function
createFunction :: (Typeable f) => f -> Function
createFunction f =
  let dynType = toDyn f
   in Function dynType (describeFunction f)

-- | Description of a 'Function' with input types and output type
data FunctionDescription = FunctionDescription
  { _inputTypes :: [Text],
    _outputType :: Text
  }
  deriving (Eq, Show)

-- | Describe a 'Function' (which doesn't have a 'Show' instance)
--   that can be put in the Registry
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

-- | A Typed value or function can be added to a Registry
--   It is either a value, having both 'Show' and 'Typeable' information
--   or a function having just 'Typeable' information
data Typed a
  = TypedValue Value
  | TypedFunction Function

-- | This is a list of functions (or "constructors") available for constructing values
newtype Functions = Functions [Function] deriving (Show, Semigroup, Monoid)

-- | Display a list of constructors
describeFunctions :: Functions -> Text
describeFunctions (Functions fs) =
  if P.null fs
    then ""
    else unlines (funDescriptionToText . funDescription <$> fs)

-- | Add one more Function to the list of Functions
addFunction :: Function -> Functions -> Functions
addFunction f (Functions fs) = Functions (f : fs)

-- | List of values available which can be used as parameters to
--   constructors for building other values
newtype Values = Values {unValues :: [Value]} deriving (Show, Semigroup, Monoid)

-- | Display a list of values
describeValues :: Values -> Text
describeValues (Values vs) =
  if P.null vs
    then ""
    else unlines (valDescriptionToText . valDescription <$> vs)

-- | Add one more Value to the list of Values
addValue :: Value -> Values -> Values
addValue v (Values vs) = Values (v : vs)

-- | The types of values that we are trying to build at a given moment
--   of the resolution algorithm.
--   We also store the function requiring a given value type to provide
--   better error messages
--   IMPORTANT: this is a *stack*, the deepest elements in the value
--   graph are first in the list
newtype Context = Context
  { _contextStack :: [(SomeTypeRep, Maybe SomeTypeRep)]
  }
  deriving (Eq, Show)

instance Semigroup Context where
  Context c1 <> Context c2 = Context (c1 <> c2)

instance Monoid Context where
  mempty = Context mempty
  mappend = (<>)

-- | Return the target types for a given context
contextTypes :: Context -> [SomeTypeRep]
contextTypes (Context cs) = fmap fst cs

-- | The values that a value depends on
newtype Dependencies = Dependencies
  { unDependencies :: [Value]
  }
  deriving (Eq, Show, Semigroup, Monoid)

-- | The values types that a value depends on
newtype DependenciesTypes = DependenciesTypes
  { unDependenciesTypes :: [SomeTypeRep]
  }
  deriving (Eq, Show, Semigroup, Monoid)

-- | Return the types of all the dependencies
dependenciesTypes :: Dependencies -> DependenciesTypes
dependenciesTypes (Dependencies ds) = DependenciesTypes (valueDynTypeRep <$> ds)

-- | The dependencies of a value + the value itself
dependenciesOn :: Value -> Dependencies
dependenciesOn value = Dependencies $ value : (unDependencies . valueDependencies $ value)

-- | Specification of values which become available for
--   construction when a corresponding type comes in context
newtype Specializations = Specializations
  { unSpecializations :: [Specialization]
  }
  deriving (Show, Semigroup, Monoid)

-- | A specialization is defined by
--   a path of types, from top to bottom in the
--    value graph and target value, which is the
--   value to use when we need a value on that type
--   on that path.
--   For example:
--      specializationPath = [App, PaymentEngine, TransactionRepository]
--      specializationValue = DatabaseConfig "localhost" 5432
--   This means that need to use this DatabaseConfig whenever
--   trying to find inputs needed to create a TransactionRepository
--   if that repository is necessary to create a PaymentEngine, itself
--   involved in the creation of the App
data Specialization = Specialization
  { _specializationPath :: SpecializationPath,
    _specializationValue :: Value
  }
  deriving (Eq, Show)

-- | List of consecutive types used when making a specific values
--   See the comments on 'Specialization'
type SpecializationPath = NonEmpty SomeTypeRep

-- | Return the various specialization paths which have possibly led to the
--   creation of that value
specializationPaths :: Value -> Maybe [SpecializationPath]
specializationPaths v =
  case mapMaybe valueSpecialization (unDependencies $ dependenciesOn v) of
    [] -> Nothing
    ss -> Just (_specializationPath <$> ss)

-- | First type of a specialization
specializationStart :: Specialization -> SomeTypeRep
specializationStart = NonEmpty.head . _specializationPath

-- | Last type of a specialization
specializationEnd :: Specialization -> SomeTypeRep
specializationEnd = NonEmpty.last . _specializationPath

-- | Return the type of the replaced value in a specialization
specializationTargetType :: Specialization -> SomeTypeRep
specializationTargetType = valueDynTypeRep . _specializationValue

-- | This represents the full context in which a value has been specialized
--   Context is the full list of types leading to the creation of that value
--   and Specialization is a sub path describing under which types the value must be specialized
--   For example, when creating a FilePath used by a Logger the context could be: App -> Database -> Sql -> Logger
--   and the Specialization just Database -> Logger
--   to specify that the file path must have a specific value in that case
data SpecializationContext = SpecializationContext { scContext :: Context, scSpecialization :: Specialization } deriving (Eq, Show)

-- | A specialization is applicable to a context if all its types
--   are part of that context, in the right order
isContextApplicable :: Context -> Specialization -> Bool
isContextApplicable context (Specialization specializationPath _value) =
  P.all (`elem` contextTypes context) specializationPath

-- | Return the specializations valid in a given context
--   Those are the specializations which path is a subpath of the current context
applicableTo :: Specializations -> Context -> Specializations
applicableTo (Specializations ss) context =
  Specializations (P.filter (isContextApplicable context) ss)

-- | The depth of a specialization in a context is the
--   the index of the "deepest" type of that specialization
--   in the stack of types of that context
--   is the one having its "deepest" type (in the value graph)
--     the "deepest" in the current context
--   If there is a tie we take the "highest" highest type of each
specializationRange :: Context -> Specialization -> SpecializationRange
specializationRange context specialization =
  SpecializationRange
    (specializationStart specialization `elemIndex` contextTypes context)
    (specializationEnd specialization `elemIndex` contextTypes context)

-- | For a given context this represents the position of a specialization path
--   in that context. startRange is the index of the start type of the specialization
--   endRange is the index of the last type.
data SpecializationRange = SpecializationRange
  { _startRange :: Maybe Int,
    _endRange :: Maybe Int
  }
  deriving (Eq, Show)

-- | A specialization range is preferable to another one if its types
--   are more specific (or "deepest" in the value graph) than the other
--   If a path is limited to just one type then a path ending with the same
--   type but specifying other types will take precedence
--   See TypesSpec for some concrete examples.
instance Ord SpecializationRange where
  SpecializationRange s1 e1 <= SpecializationRange s2 e2
    | e1 /= s1 && e2 /= s2 = e1 <= e2 || (e1 == e2 && s1 <= s2)
    | e1 == s1 && e2 /= s2 = e1 < e2
    | otherwise = e1 <= e2

-- | In a given context, create a value as specified by a specialization
--   the full context is necessary since the specificationPath is
--   only a subpath of a given creation context
--   Note: there are no dependencies for this value since it has been directly
--   provided by a Specialization
createValueFromSpecialization :: Context -> Specialization -> Value
createValueFromSpecialization context specialization@(Specialization _ (ProvidedValue d desc)) =
  -- the creation context for that value
  CreatedValue d desc (Just (SpecializationContext context specialization)) mempty
-- this is not supposed to happen since specialization are always
-- using ProvidedValues
createValueFromSpecialization _ v = _specializationValue v

-- | Display a list of specializations for the Registry, just showing the
--   context (a type) in which a value must be selected
describeSpecializations :: Specializations -> Text
describeSpecializations (Specializations ss) =
  if P.null ss
    then ""
    else "specializations\n" <> unlines (P.show <$> ss)

-- | List of functions modifying some values right after they have been
--   built. This enables "tweaking" the creation process with slightly
--   different results. Here SomeTypeRep is the target value type a and
newtype Modifiers = Modifiers [(SomeTypeRep, ModifierFunction)] deriving (Semigroup, Monoid)

-- | A ModifierFunction modifies an already created value
--   If that value has been created as the result of a specialization
--   then the specialization path is also passed to the function
--   This is used for memoizing actions using a cache so that we
--   cache each specialized value separately.
type ModifierFunction = Maybe [SpecializationPath] -> Function

-- | Create a 'ModifierFunction' value from a Haskell function
--   The application of that function does not depend on the fact
--   that we are trying to apply it to a specialized value
createConstModifierFunction :: (Typeable f) => f -> ModifierFunction
createConstModifierFunction f = const (createFunction f)

instance Show Modifiers where
  show = toS . describeModifiers

-- | Display a list of modifiers for the Registry, just showing the
--   type of the modified value
describeModifiers :: Modifiers -> Text
describeModifiers (Modifiers ms) =
  if P.null ms
    then ""
    else "modifiers for types\n" <> unlines (P.show . fst <$> ms)
