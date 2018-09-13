{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE UndecidableInstances  #-}

{-
  A registry supports the creation of values out of existing values and
  functions.

  It contains 4 parts:
   - values: they are available for building anything else and have their exact value can be shown
   - functions: they are used to build other values. Only their type can be shown
   - specializations: description of specific values to use while trying to build another value of a given type
   - modifiers: function to apply to a newly built value before storing it for future use

  A registry is created by using the +: operator, adding functions or values to the empty `end` registry:

    registry =
         val (Config 1)
      +: val "hello"
      +: fun add1
      +: fun show1
      +: end

  At the type level a list of all the function inputs and all the outputs is being kept to
  allow some checks to be made when we want to build a value out of the registry.

  Registries have a `Monoid` instance so they can be created incrementally:

    config =
         val (Config 1)
      +: val "hello"
      +: end

    constructors =
      +: fun add1
      +: fun show1
      +: end

    registry =
      config <> constructors

  It is also possible to use the `<>` operator to "override" some configurations:

    mocks =
         fun noLogging
      +: fun inMemoryDb
      +: end

    mocks <> registry

-}
module Data.Registry.Registry where

import           Data.Registry.Internal.Cache
import           Data.Registry.Internal.Dynamic
import           Data.Registry.Internal.Registry
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
--   Internally all functions and values are stored as Dynamic values
--   so that we can access their representation
data Registry (inputs :: [*]) (outputs :: [*]) =
  Registry {
    _values          :: Values
  , _functions       :: Functions
  , _specializations :: Specializations
  , _modifiers       :: Modifiers
  }

instance Show (Registry inputs outputs) where
  show (Registry (Values vs) (Functions fs) _ _) =
    let showValues =
          if null vs then ""
          else            unlines (_description <$> vs)
        showFunctions =
            if null fs then ""
            else            unlines (_description <$> fs)
    in
        toS $ unlines [showValues, showFunctions]

instance Semigroup (Registry inputs outputs) => Monoid (Registry inputs outputs) where
  mempty = Registry (Values []) (Functions []) (Specializations []) (Modifiers [])
  mappend = (<>)

(<+>) :: Registry is1 os1 -> Registry is2 os2 -> Registry (is1 :++ is2) (os1 :++ os2)
(<+>) (Registry (Values vs1) (Functions fs1) (Specializations ss1) (Modifiers ms1))
       (Registry (Values vs2) (Functions fs2) (Specializations ss2) (Modifiers ms2))  =
          Registry (Values (vs1 <> vs2)) (Functions (fs1 <> fs2)) (Specializations (ss1 <> ss2)) (Modifiers (ms1 <> ms2))

-- | Store an element in the registry
--   Internally elements are stored as dynamic values
register
  :: (Typeable a)
  => Typed a
  -> Registry ins out
  -> Registry (Inputs a :++ ins) (Output a ': out)
register typed@(Typed a _) (Registry (Values vs) (Functions fs) specializations modifiers) =
  if isFunction a then
    Registry (Values vs) (Functions (toUntyped typed : fs)) specializations modifiers
  else
    Registry (Values (toUntyped typed : vs)) (Functions fs) specializations modifiers

-- | Add an element to the Registry - Alternative to register where the parentheses can be ommitted
infixr 5 +:
(+:) :: (Typeable a) => Typed a -> Registry ins out -> Registry (Inputs a :++ ins) (Output a ': out)
(+:) = register

-- | The empty Registry
end :: Registry '[] '[]
end = Registry (Values []) (Functions []) (Specializations []) (Modifiers [])

val :: (Typeable a, Show a) => a -> Typed a
val a = Typed (toDyn a) (describeValue a)

valTo :: forall m a . (Applicative m, Typeable a, Typeable (m a), Show a) => a -> Typed (m a)
valTo a = Typed (toDyn (pure a :: m a)) (describeValue a)

fun :: (Typeable a) => a -> Typed a
fun a =
  let dynType = toDyn a
  in  Typed dynType (describeFunction a)

-- | This is just a shortcut to (fun . allTo)
funTo :: forall m a b . (ApplyVariadic m a b, Typeable a, Typeable b) => a -> Typed b
funTo a = fun (allTo @m a)

-- | This is just a shortcut to (fun . argsTo)
funAs :: forall m a b . (ApplyVariadic1 m a b, Typeable a, Typeable b) => a -> Typed b
funAs a = fun (argsTo @m a)

-- | For a given type `a` being currently built
--   when a value of type `b` is required pass a specific
--   value
specialize
  :: forall a b ins out
   . (Typeable a, Contains a out, Typeable b)
  => b
  -> Registry ins out
  -> Registry ins out
specialize b (Registry values functions (Specializations c) modifiers) = Registry
  values
  functions
  (Specializations ((someTypeRep (Proxy :: Proxy a), toDyn b) : c))
  modifiers

-- | Once a value has been computed allow to modify it before storing
--   it
tweak
  :: forall a ins out
   . (Typeable a, Contains a out)
  => (a -> a)
  -> Registry ins out
  -> Registry ins out
tweak f (Registry values functions specializations (Modifiers mf)) = Registry values functions specializations
  (Modifiers ((someTypeRep (Proxy :: Proxy a), toDyn f) : mf))

-- | Return singleton values for a monadic type
singleton
  :: forall m a ins out
   . (MonadIO m, Typeable a, Typeable (m a), Contains (m a) out)
  => Registry ins out
  -> IO (Registry ins out)
singleton r = do
  cache <- newCache @a
  pure $ tweak @(m a) (fetch cache) r
