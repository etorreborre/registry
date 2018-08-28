{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
  This module offers some utilities to register
  functions to build values of a given type

  With a given register it is possible to ask to "make"
  a value of a given type. If there is already a value of that type
  that value is returned. Otherwise if there is a function having the
  desired return type the `make` function will try to "make" all the
  inputs to the function, using the registry and then will call the
  function to create a value of that type.

  For example given a
    `Registry '[Int, Int -> Bool -> Double, Bool, Double -> Text]`

  it is possible to build a value of type `Text` by using the first
  `Int`, the `Bool`, pass them to the `Int -> Bool -> Double` function
  then use the `Double -> Text` function to get a value of type `Text`.

  The only requirement for values used by a registry is that they should be `Typeable`.

  See the tests for some examples of use of the API.

-}
module Data.Box.Make
  ( Registry(..)
  , module Data.Box.Solver
  , end
  , fun
  , make
  , makeUnsafe
  , register
  , (+:)
  , specialize
  , tweak
  , val
  )
where

import           Data.Box.Solver
import           Data.Dynamic
import           Data.Text       as T (drop, dropEnd, unlines)
import           Data.Typeable   (Typeable)
import qualified Prelude         (error, show)
import           Protolude       as P hiding (Constructor)
import           Type.Reflection

-- | Container for a list of functions or values
--   Internally all functions and values are stored as Dynamic values
--   so that we can access their representation
data Registry (inputs :: [*]) (outputs :: [*]) =
  Registry {
    _overrides :: Overrides
  , _modifiers :: Modifiers
  , _values    :: Values
  , _functions :: Functions
}

instance Show (Registry inputs outputs) where
  show (Registry _ _ (Values vs) (Functions fs)) =
    let showValues =
          if null vs then ""
          else            unlines (_description <$> vs)
        showFunctions =
            if null fs then ""
            else            unlines (_description <$> fs)
    in
        toS $ unlines [showValues, showFunctions]

instance Semigroup (Registry inputs outputs) => Monoid (Registry inputs outputs) where
  mempty = Registry (Overrides []) (Modifiers []) (Values []) (Functions [])
  mappend = (<>)

instance Semigroup (Registry inputs outputs) where
  (<>) (Registry (Overrides os1) (Modifiers ms1) (Values vs1) (Functions fs1))
       (Registry (Overrides os2) (Modifiers ms2) (Values vs2) (Functions fs2))  =
          Registry (Overrides (os1 <> os2)) (Modifiers (ms1 <> ms2)) (Values (vs1 <> vs2)) (Functions (fs1 <> fs2))

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
newtype Overrides = Overrides [(SomeTypeRep, Dynamic)] deriving (Show)

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
register (Typed a t) (Registry overrides modifiers (Values vs) (Functions fs)) =
  if isFunction a then
    Registry overrides modifiers (Values vs) (Functions (Untyped a t : fs))
  else
    Registry overrides modifiers (Values (Untyped a t : vs)) (Functions fs)

-- | Add an element to the Registry - Alternative to register where the parentheses can be ommitted
infixr 5 +:
(+:) :: (Typeable a) => Typed a -> Registry ins out -> Registry (Union (Inputs a) ins) (Union '[Output a] out)
(+:) = register

-- | The empty Registry
end :: Registry '[] '[]
end = Registry (Overrides []) (Modifiers []) (Values []) (Functions [])

val :: (Typeable a, Show a) => a -> Typed a
val a = Typed (toDyn a) (show a)

fun :: (Typeable a) => a -> Typed a
fun a =
  let dynType = toDyn a
  in  Typed dynType (T.drop 2 $ T.dropEnd 2 $ show dynType)


-- | For a given type `a` being currently built
--   when a value of type `b` is required pass a specific
--   value
specialize
  :: forall a b ins out
   . (Typeable a, Contains a out, Typeable b)
  => b
  -> Registry ins out
  -> Registry ins out
specialize b (Registry (Overrides c) modifiers values functions) = Registry
  (Overrides ((someTypeRep (Proxy :: Proxy a), toDyn b) : c))
  modifiers
  values
  functions

-- | Once a value has been computed allow to modify it before storing
--   it
tweak
  :: forall a ins out
   . (Typeable a, Contains a out)
  => (a -> a)
  -> Registry ins out
  -> Registry ins out
tweak f (Registry overrides (Modifiers mf) values functions) = Registry
  overrides
  (Modifiers ((someTypeRep (Proxy :: Proxy a), toDyn f) : mf))
  values
  functions

-- | For a given registry make an element of type a
--   We want to ensure that a is indeed one of the return types
make
  :: forall a ins out
   . (Typeable a, Contains a out, Solvable ins out)
  => Registry ins out
  -> a
make = makeUnsafe

-- | This version of make only execute checks at runtime
--   this can speed-up compilation when writing tests or in ghci
makeUnsafe :: forall a ins out . (Typeable a) => Registry ins out -> a
makeUnsafe registry =
  let values       = _values registry
      functions    = _functions registry
      overrides    = _overrides registry
      modifiers    = _modifiers registry
      targetType   = someTypeRep (Proxy :: Proxy a)
  in
      -- | use the makeUntyped function to create an element of the target type from a list of values and functions
      --   the list of values is kept as some State so that newly created values can be added to the current state
      case
        evalState
          (makeUntyped targetType (Context [targetType]) functions overrides modifiers)
          values
      of
        Nothing -> Prelude.error
          ("could not create a " <> show targetType <> " out of the registry")

        Just result -> fromMaybe
          (Prelude.error ("could not cast the computed value to a " <> show targetType <> ". The value is of type: " <> show (dynTypeRep result)))
          (fromDynamic result)



-- * Private - WARNING: HIGHLY UNTYPED IMPLEMENTATION !

-- | Make a value from a desired output type represented by SomeTypeRep
--   and a list of possible constructors
--   A context is passed in the form of a stack of the types we are trying to build so far
makeUntyped
  :: SomeTypeRep
  -> Context
  -> Functions
  -> Overrides
  -> Modifiers
  -> State Values (Maybe Dynamic)
makeUntyped targetType context functions overrides modifiers = do
  values <- get

  -- is there already a value with the desired type?
  case findValue targetType context overrides values of
    Nothing ->
      -- if not, is there a way to build such value?
      case findConstructor targetType functions of
        Nothing -> pure Nothing

        Just c  -> do
          let inputTypes = collectInputTypes c
          inputs <- makeInputs inputTypes context functions overrides modifiers

          if length inputs /= length inputTypes
            then
              Prelude.error
              $  toS
              $  unlines
              $  ["could not make all the inputs for ", show c, ". Only "]
              <> (show <$> inputs)
              <> ["could be made"]
            else do
              let v = applyFunction c inputs
              modified <- storeValue modifiers v
              pure (Just modified)


    Just v -> do
      modified <- storeValue modifiers v
      pure (Just modified)

-- | If Dynamic is a function collect all its input types
collectInputTypes :: Dynamic -> [SomeTypeRep]
collectInputTypes = go . dynTypeRep
 where
  go :: SomeTypeRep -> [SomeTypeRep]
  go (SomeTypeRep (Fun in1 out)) = SomeTypeRep in1 : go (SomeTypeRep out)
  go _                           = []

-- | Make the input values of a given function
--   When a value has been made it is placed on top of the
--   existing registry so that it is memoized if needed in
--   subsequent calls
makeInputs
  :: [SomeTypeRep]          -- inputs to make
  -> Context                -- context
  -> Functions              -- overrides
  -> Overrides              -- overrides
  -> Modifiers              -- modifiers
  -> State Values [Dynamic] -- list of made values
makeInputs [] _ _ _ _ = pure []

makeInputs (i : ins) (Context context) functions overrides modifiers =
  reverse <$> if i `elem` context
    then
      Prelude.error
      $  toS
      $  unlines
      $  ["cycle detected! The current types being built are "]
      <> (show <$> context)
      <> ["But we are trying to build again " <> show i]
    else do
      madeInput <- makeUntyped i (Context (i : context)) functions overrides modifiers
      case madeInput of
        Nothing ->
          -- if one input cannot be made iterate with the rest for better reporting
          -- of what could be eventually made
          makeInputs ins (Context context) functions overrides modifiers

        Just v ->
          (v :) <$> makeInputs ins (Context context) functions overrides modifiers

storeValue :: Modifiers -> Dynamic -> State Values Dynamic
storeValue (Modifiers ms) value =
  let valueToStore = case findModifier ms of
        Nothing     -> value
        Just (_, f) -> applyFunction f [value]
  in  modify (addValue (Untyped valueToStore (show . dynTypeRep $ value))) >>
      pure valueToStore
  where findModifier = find (\(m, _) -> dynTypeRep value == m)

-- | Apply a Dynamic function to a list of Dynamic values
applyFunction
  :: Dynamic    -- function
  -> [Dynamic]  -- inputs
  -> Dynamic    -- result
applyFunction f [] =
  Prelude.error
    $  "the function "
    <> show (dynTypeRep f)
    <> " cannot be applied to an empty list of parameters"
applyFunction f [i     ] = dynApp f i
applyFunction f (i : is) = applyFunction (dynApp f i) is

isFunction :: Dynamic -> Bool
isFunction d =
  case dynTypeRep d of
    SomeTypeRep (Fun _ _) -> True
    _                     -> False

-- | Find a value having a target type
--   from a list of dynamic values found in a list of constructors
--   where some of them are not functions
--   There is also a list of overrides when we can specialize the values to use
--   if a given type is part of the context
findValue
  :: SomeTypeRep
  -> Context
  -> Overrides
  -> Values
  -> Maybe Dynamic
-- no overrides or constructors to choose from
findValue _ _ (Overrides []) (Values []) = Nothing

-- recurse on the overrides first
findValue target (Context context) (Overrides ((t, v) : rest)) values =
  -- if there is an override which value matches the current target
  -- and if that override is in the current context then return the value
  if target == dynTypeRep v && t `elem` context then
    Just v
  else
    findValue target (Context context) (Overrides rest) values

-- otherwise recurse on the list of constructors until a value
-- with the target type is found
findValue target context overrides (Values (Untyped t _ : rest)) =
  if dynTypeRep t == target then
    Just t
  else
    findValue target context overrides (Values rest)

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

-- | If the input type is a function type return its output type
outputType :: SomeTypeRep -> SomeTypeRep
outputType (SomeTypeRep (Fun _ out)) = outputType (SomeTypeRep out)
outputType r                         = r
