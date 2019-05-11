{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE UndecidableInstances  #-}

{- |
  A registry supports the creation of values out of existing values and
  functions.

  It contains 4 parts:

  * values: they are available for building anything else and have their exact value can be shown
  * functions: they are used to build other values. Only their type can be shown
  * specializations: description of specific values to use while trying to build another value of a given type
  * modifiers: function to apply to a newly built value before storing it for future use

  A registry is created by using the `+:` operator, adding functions or values to the empty `end` registry:

  > registry =
  >      val (Config 1)
  >   +: val "hello"
  >   +: fun add1
  >   +: fun show1
  >   +: end

  At the type level a list of all the function inputs and all the outputs is being kept to
  allow some checks to be made when we want to build a value out of the registry.

  It is possible to use the `<+>` operator to "override" some configurations:

  >  mocks =
  >       fun noLogging
  >    +: fun inMemoryDb
  >    +: end
  >
  >  mocks <+> registry

-}
module Data.Registry.Registry where

import           Data.Registry.Internal.Cache
import           Data.Registry.Internal.Types
import           Data.Registry.Lift
import           Data.Registry.Solver
import           Data.Dynamic
import           Data.Semigroup             ((<>))
import           Data.Text                  as T (unlines)
import           Data.Typeable              (Typeable)
import qualified Prelude                    (show)
import           Protolude                  as P hiding ((<>))
import           Type.Reflection

-- | Container for a list of functions or values
--   Internally all functions and values are stored as 'Dynamic' values
--   so that we can access their representation
data Registry (inputs :: [*]) (outputs :: [*]) =
  Registry {
    _values          :: Values
  , _functions       :: Functions
  , _specializations :: Specializations
  , _modifiers       :: Modifiers
  }

instance Show (Registry inputs outputs) where
  show (Registry vs fs ss ms) =
    toS $ unlines [
        describeValues vs
      , describeFunctions fs
      , describeSpecializations ss
      , describeModifiers ms
      ]

instance Semigroup (Registry inputs outputs) where
  (<>) (Registry (Values vs1) (Functions fs1) (Specializations ss1) (Modifiers ms1))
       (Registry (Values vs2) (Functions fs2) (Specializations ss2) (Modifiers ms2)) =
         Registry (Values (vs1 <> vs2)) (Functions (fs1 <> fs2)) (Specializations (ss1 <> ss2)) (Modifiers (ms1 <> ms2))

instance Semigroup (Registry inputs outputs) => Monoid (Registry inputs outputs) where
  mempty = Registry (Values []) (Functions []) (Specializations []) (Modifiers [])
  mappend = (<>)

-- | Append 2 registries together
infixr 4 <+>
(<+>) :: Registry is1 os1 -> Registry is2 os2 -> Registry (is1 :++ is2) (os1 :++ os2)
(<+>) (Registry (Values vs1) (Functions fs1) (Specializations ss1) (Modifiers ms1))
       (Registry (Values vs2) (Functions fs2) (Specializations ss2) (Modifiers ms2))  =
          Registry (Values (vs1 <> vs2)) (Functions (fs1 <> fs2)) (Specializations (ss1 <> ss2)) (Modifiers (ms1 <> ms2))

-- | Store an element in the registry
--   Internally elements are stored as 'Dynamic' values
register :: (Typeable a)
  => Typed a
  -> Registry ins out
  -> Registry (Inputs a :++ ins) (Output a ': out)
register (TypedValue v) (Registry (Values vs) functions specializations modifiers) =
  Registry (Values (v : vs)) functions specializations modifiers

register (TypedFunction f) (Registry (Values vs) (Functions fs) specializations modifiers) =
  Registry (Values vs) (Functions (f : fs)) specializations modifiers

-- | Add an element to the Registry - Alternative to register where the parentheses can be ommitted
infixr 5 +:
(+:) :: (Typeable a) => Typed a -> Registry ins out -> Registry (Inputs a :++ ins) (Output a ': out)
(+:) = register

-- Unification of +: and <+>
infixr 5 <:
class AddRegistryLike a b c | a b -> c where
  (<:) :: a -> b -> c

instance (insr ~ (ins1 :++ ins2), outr ~ (out1 :++ out2)) => AddRegistryLike (Registry ins1 out1) (Registry ins2 out2) (Registry insr outr) where
  (<:) = (<+>)

instance (Typeable a, insr ~ (Inputs a :++ ins2), outr ~ (Output a : out2)) => AddRegistryLike (Typed a) (Registry ins2 out2) (Registry insr outr) where
  (<:) = register

instance (Typeable a, insr ~ (Inputs a :++ ins2), outr ~ (Output a : out2)) => AddRegistryLike (Registry ins2 out2) (Typed a) (Registry insr outr) where
  (<:) = flip register

instance (Typeable a, Typeable b, insr ~ (Inputs a :++ (Inputs b :++ '[])), outr ~ (Output a : '[Output b])) => AddRegistryLike (Typed a) (Typed b) (Registry insr outr) where
  (<:) a b = register a (register b end)

-- | Make the lists of types in the Registry unique, either for better display
--   or for faster compile-time resolution with the make function
normalize :: Registry ins out -> Registry (Normalized ins) (Normalized out)
normalize (Registry vs fs ss ms) = Registry vs fs ss ms

-- | The empty Registry
end :: Registry '[] '[]
end = Registry (Values []) (Functions []) (Specializations []) (Modifiers [])

-- | Create a value which can be added to the 'Registry'
val :: (Typeable a, Show a) => a -> Typed a
val a = TypedValue (ProvidedValue (toDyn a) (describeValue a))

-- | Create a value which can be added to the 'Registry' and "lift" it to an 'Applicative' context
valTo :: forall m a . (Applicative m, Typeable a, Typeable (m a), Show a) => a -> Typed (m a)
valTo a = TypedValue (liftProvidedValue @m a)

-- | Create a "lifted" a Value
liftProvidedValue :: forall m a . (Applicative m, Typeable a, Typeable (m a), Show a) => a -> Value
liftProvidedValue a = ProvidedValue (toDyn (pure a :: m a)) (describeValue a)

-- | Create a function which can be added to the 'Registry'
fun :: (Typeable a) => a -> Typed a
fun a = TypedFunction (createFunction a)

-- | This is a shortcut to @fun . allTo@ where @allTo@ lifts all the inputs and output
--   to an 'Applicative' context
funTo :: forall m a b . (ApplyVariadic m a b, Typeable a, Typeable b) => a -> Typed b
funTo a = fun (allTo @m a)

-- | This is a shortcut to @fun . argsTo@ where @allTo@ lifts all the inputs
--   to an Applicative context
funAs :: forall m a b . (ApplyVariadic1 m a b, Typeable a, Typeable b) => a -> Typed b
funAs a = fun (argsTo @m a)

-- | For a given type @a@ being currently built
--   when a value of type @b@ is required pass a specific value
specialize :: forall a b ins out . (Typeable a, Contains a out, Typeable b)
  => b
  -> Registry ins out
  -> Registry ins out
specialize = specializeUnsafe @a @b @ins @out

specializePath :: forall path b ins out . (PathToTypeReps path, IsSubset path out, Typeable b)
  => b
  -> Registry ins out
  -> Registry ins out
specializePath = specializePathUnsafe @path @b @ins @out

-- | This is similar to specialize but additionally uses the 'Show' instance of @b@
--   to display more information when printing the registry out
specializeVal :: forall a b ins out . (Typeable a, Contains a out, Typeable b, Show b)
  => b
  -> Registry ins out
  -> Registry ins out
specializeVal = specializeUnsafeVal @a @b @ins @out

specializePathVal :: forall path b ins out . (PathToTypeReps path, IsSubset path out, Typeable b, Show b)
  => b
  -> Registry ins out
  -> Registry ins out
specializePathVal = specializePathUnsafeVal @path @b @ins @out

specializeValTo :: forall m a b ins out . (Applicative m, Typeable a, Contains a out, Typeable (m b), Typeable b, Show b)
  => b
  -> Registry ins out
  -> Registry ins out
specializeValTo = specializeUnsafeValTo @m @a @b @ins @out

specializePathValTo :: forall m path b ins out . (Applicative m, PathToTypeReps path, IsSubset path out, Typeable (m b), Typeable b, Show b)
  => b
  -> Registry ins out
  -> Registry ins out
specializePathValTo = specializePathUnsafeValTo @m @path @b @ins @out

-- | For a given type `a` being currently built
--   when a value of type `b` is required pass a specific
--   value
specializeUnsafe :: forall a b ins out . (Typeable a, Typeable b)
  => b
  -> Registry ins out
  -> Registry ins out
specializeUnsafe b (Registry values functions (Specializations c) modifiers) = Registry
  values
  functions
  (Specializations (Specialization (pure $ someTypeRep (Proxy :: Proxy a)) (createTypeableValue b) : c))
  modifiers

specializePathUnsafe :: forall path b ins out . (PathToTypeReps path, Typeable b)
  => b
  -> Registry ins out
  -> Registry ins out
specializePathUnsafe b (Registry values functions (Specializations c) modifiers) = Registry
  values
  functions
  (Specializations (Specialization (someTypeReps (Proxy :: Proxy path)) (createTypeableValue b) : c))
  modifiers

specializeUnsafeVal :: forall a b ins out . (Typeable a, Contains a out, Typeable b, Show b)
  => b
  -> Registry ins out
  -> Registry ins out
specializeUnsafeVal b (Registry values functions (Specializations c) modifiers) = Registry
  values
  functions
  (Specializations (Specialization (pure $ someTypeRep (Proxy :: Proxy a)) (createValue b) : c))
  modifiers

specializePathUnsafeVal :: forall path b ins out . (PathToTypeReps path, Typeable b, Show b)
  => b
  -> Registry ins out
  -> Registry ins out
specializePathUnsafeVal b (Registry values functions (Specializations c) modifiers) = Registry
  values
  functions
  (Specializations (Specialization (someTypeReps (Proxy :: Proxy path)) (createValue b) : c))
  modifiers

specializeUnsafeValTo :: forall m a b ins out . (Applicative m, Typeable a, Typeable (m b), Typeable b, Show b)
  => b
  -> Registry ins out
  -> Registry ins out
specializeUnsafeValTo b (Registry values functions (Specializations c) modifiers) = Registry
  values
  functions
  (Specializations (Specialization (pure $ someTypeRep (Proxy :: Proxy a)) (liftProvidedValue @m b) : c))
  modifiers

specializePathUnsafeValTo :: forall m path b ins out . (Applicative m, PathToTypeReps path, Typeable (m b), Typeable b, Show b)
  => b
  -> Registry ins out
  -> Registry ins out
specializePathUnsafeValTo b (Registry values functions (Specializations c) modifiers) = Registry
  values
  functions
  (Specializations (Specialization (someTypeReps (Proxy :: Proxy path)) (liftProvidedValue @m b) : c))
  modifiers

-- | Typeclass for extracting type representations out of a list of types
class PathToTypeReps (path :: [*]) where
  someTypeReps :: Proxy path -> NonEmpty SomeTypeRep

instance {-# OVERLAPPING #-} (Typeable a) => PathToTypeReps '[a] where
  someTypeReps = const $ pure (someTypeRep (Proxy :: Proxy a))

instance (Typeable a, PathToTypeReps rest) => PathToTypeReps (a : rest) where
  someTypeReps = const $ someTypeRep (Proxy :: Proxy a) :| toList (someTypeReps (Proxy :: Proxy rest))

-- | Once a value has been computed allow to modify it before storing it
--   This keeps the same registry type
tweak :: forall a ins out . (Typeable a, Contains a out)
  => (a -> a)
  -> Registry ins out
  -> Registry ins out
tweak = tweakUnsafe

-- | Once a value has been computed allow to modify it before storing
--   it
tweakUnsafe :: forall a ins out . (Typeable a)
  => (a -> a)
  -> Registry ins out
  -> Registry ins out
tweakUnsafe f (Registry values functions specializations (Modifiers mf)) = Registry values functions specializations
  (Modifiers ((someTypeRep (Proxy :: Proxy a), createConstModifierFunction f) : mf))

-- * Memoization

-- | Instantiating components can trigger side-effects
--   The way the resolution algorithm works a component of type `m a` will be
--   re-executed *everytime* it is needed as a given dependency
--   This section adds support for memoizing those actions (component creation + optional warmup)

-- | Return memoized values for a monadic type
--   Note that the returned Registry is in 'IO' because we are caching a value
--   and this is a side-effect!
memoize :: forall m a ins out . (MonadIO m, Typeable a, Typeable (m a), Contains (m a) out)
  => Registry ins out
  -> IO (Registry ins out)
memoize = memoizeUnsafe @m @a @ins @out

-- | Memoize an action for a given type but don't check if the value is part of the registry outputs
memoizeUnsafe :: forall m a ins out . (MonadIO m, Typeable a, Typeable (m a))
  => Registry ins out
  -> IO (Registry ins out)
memoizeUnsafe (Registry values functions specializations (Modifiers mf)) = do
  cache <- newCache @a
  let modifiers = Modifiers ((someTypeRep (Proxy :: Proxy (m a)), \key -> createFunction (fetch @a @m cache key)) : mf)
  pure $ Registry values functions specializations modifiers

-- | Memoize *all* the output actions of a Registry when they are creating effectful components
--   This relies on a helper data structure `MemoizeRegistry` tracking the types already
--   memoized and a typeclass MemoizedActions going through the list of `out` types to process them
--   one by one. Note that a type of the form `a` will not be memoized (only `m a`)
memoizeAll :: forall m ins out . (MonadIO m, MemoizedActions out) => Registry ins out -> IO (Registry ins out)
memoizeAll r = _unMemoizeRegistry <$>
  memoizeActions (startMemoizeRegistry r)

newtype MemoizeRegistry (todo :: [*]) (ins :: [*]) (out :: [*]) = MemoizeRegistry { _unMemoizeRegistry :: Registry ins out }

startMemoizeRegistry :: Registry ins out -> MemoizeRegistry out ins out
startMemoizeRegistry = MemoizeRegistry

makeMemoizeRegistry :: forall todo ins out . Registry ins out -> MemoizeRegistry todo ins out
makeMemoizeRegistry = MemoizeRegistry @todo

class MemoizedActions ls where
  memoizeActions :: MemoizeRegistry ls ins out -> IO (MemoizeRegistry '[] ins out)

instance MemoizedActions '[] where
  memoizeActions = pure

instance {-# OVERLAPPING #-} (MonadIO m, Typeable a, Typeable (m a), MemoizedActions rest) => MemoizedActions (m a : rest) where
  memoizeActions (MemoizeRegistry r) = do
    r' <- memoizeUnsafe @m @a r
    memoizeActions (makeMemoizeRegistry @rest r')

instance (MemoizedActions rest) => MemoizedActions (a : rest) where
  memoizeActions (MemoizeRegistry r) =
    memoizeActions (makeMemoizeRegistry @rest r)

-- * DEPRECATIONS

{-# DEPRECATED singleton "use memoize instead" #-}
singleton :: forall m a ins out . (MonadIO m, Typeable a, Typeable (m a), Contains (m a) out) => Registry ins out -> IO (Registry ins out)
singleton = memoize @m @a @ins @out

{-# DEPRECATED singletonUnsafe "use memoizeUnsafe instead" #-}
singletonUnsafe :: forall m a ins out . (MonadIO m, Typeable a, Typeable (m a)) => Registry ins out -> IO (Registry ins out)
singletonUnsafe = memoizeUnsafe @m @a @ins @out
