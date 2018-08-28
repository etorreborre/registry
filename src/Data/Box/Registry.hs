{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
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
module Data.Box.Registry where

import           Data.Box.Dynamic
import           Data.Box.Lift
import           Data.Box.Solver
import           Data.Dynamic
import           Data.Semigroup   ((<>))
import           Data.Text        as T (drop, dropEnd, unlines)
import           Data.Typeable    (Typeable)
import qualified Prelude          (show)
import           Protolude        as P hiding ((<>))
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

instance Semigroup (Registry inputs outputs) where
  (<>) (Registry (Values vs1) (Functions fs1) (Specializations ss1) (Modifiers ms1))
       (Registry (Values vs2) (Functions fs2) (Specializations ss2) (Modifiers ms2))  =
          Registry (Values (vs1 <> vs2)) (Functions (fs1 <> fs2)) (Specializations (ss1 <> ss2)) (Modifiers (ms1 <> ms2))

-- List of types currently being built
newtype Context = Context [SomeTypeRep] deriving (Show)

-- List of functions available for constructing other values
newtype Functions = Functions [Untyped]

-- List of values available for constructing other values
newtype Values = Values [Untyped]

addValue :: Untyped -> Values -> Values
addValue v (Values vs) = Values (v : vs)

data Typed a = Typed Dynamic Text

instance Show (Typed a) where
  show (Typed _ t) = toS t

data Untyped = Untyped {
  _value       :: Dynamic
, _description :: Text
}

-- Specification of values which become available for
-- construction when a corresponding type comes in context
newtype Specializations = Specializations [(SomeTypeRep, Dynamic)] deriving (Show)

-- List of functions modifying some values right after they have been
-- built. This enables "tweaking" the creation process with slightly
-- different results. Here SomeTypeRep is the target value type 'a' and
-- Dynamic is an untyped function a -> a
newtype Modifiers = Modifiers [(SomeTypeRep, Dynamic)] deriving (Show)

-- | Store an element in the registry
--   Internally elements are stored as dynamic values
register
  :: (Typeable a)
  => Typed a
  -> Registry ins out
  -> Registry (Union (Inputs a) ins) (Union '[Output a] out)
register (Typed a t) (Registry (Values vs) (Functions fs) specializations modifiers) =
  if isFunction a then
    Registry (Values vs) (Functions (Untyped a t : fs)) specializations modifiers
  else
    Registry (Values (Untyped a t : vs)) (Functions fs) specializations modifiers

-- | Add an element to the Registry - Alternative to register where the parentheses can be ommitted
infixr 5 +:
(+:) :: (Typeable a) => Typed a -> Registry ins out -> Registry (Union (Inputs a) ins) (Union '[Output a] out)
(+:) = register

-- | The empty Registry
end :: Registry '[] '[]
end = Registry (Values []) (Functions []) (Specializations []) (Modifiers [])

val :: (Typeable a, Show a) => a -> Typed a
val a = Typed (toDyn a) (show a)

fun :: (Typeable a) => a -> Typed a
fun a =
  let dynType = toDyn a
  in  Typed dynType (T.drop 2 $ T.dropEnd 2 $ show dynType)

funM :: forall m a b . (Monad m, ApplyVariadic1 m a b, Typeable b) => a -> Typed b
funM a = fun (intoM @m a)

pureM :: forall m a b . (Monad m, ApplyVariadic m a b, Typeable b) => a -> Typed b
pureM a = fun (into @m a)

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
tweak f (Registry values functions specializations (Modifiers mf)) = Registry
  values
  functions
  specializations
  (Modifiers ((someTypeRep (Proxy :: Proxy a), toDyn f) : mf))

storeValue :: Modifiers -> Dynamic -> State Values Dynamic
storeValue (Modifiers ms) value =
  let valueToStore = case findModifier ms of
        Nothing     -> value
        Just (_, f) -> applyFunction f [value]
  in  modify (addValue (Untyped valueToStore (show . dynTypeRep $ value))) >>
      pure valueToStore
  where findModifier = find (\(m, _) -> dynTypeRep value == m)

-- | Find a value having a target type
--   from a list of dynamic values found in a list of constructors
--   where some of them are not functions
--   There is also a list of specializations when we can specialize the values to use
--   if a given type is part of the context
findValue
  :: SomeTypeRep
  -> Context
  -> Specializations
  -> Values
  -> Maybe Dynamic
-- no specializations or constructors to choose from
findValue _ _ (Specializations []) (Values []) = Nothing

-- recurse on the specializations first
findValue target (Context context) (Specializations ((t, v) : rest)) values =
  -- if there is an override which value matches the current target
  -- and if that override is in the current context then return the value
  if target == dynTypeRep v && t `elem` context then
    Just v
  else
    findValue target (Context context) (Specializations rest) values

-- otherwise recurse on the list of constructors until a value
-- with the target type is found
findValue target context specializations (Values (Untyped t _ : rest)) =
  if dynTypeRep t == target then
    Just t
  else
    findValue target context specializations (Values rest)

-- | Find a constructor function returning a target type
--   from a list of constructorsfe
findConstructor
  :: SomeTypeRep
  -> Functions
  -> Maybe Dynamic

findConstructor _      (Functions []        ) = Nothing
findConstructor target (Functions (Untyped c _ : rest)) =
  case dynTypeRep c of
    SomeTypeRep (Fun _ out) ->
      if outputType (SomeTypeRep out) == target then
        Just c
      else
        findConstructor target (Functions rest)

    _ -> findConstructor target (Functions rest)
