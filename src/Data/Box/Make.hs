{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}

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
module Data.Box.Make (
  Registry(..)
, module Data.Box.Lift
, module Data.Box.Solver
, end
, make
, makeUnsafe
, register
, (+:)
, specialize
) where

import           Data.Dynamic
import           Data.Box.Lift
import           Data.Box.Solver
import           Data.Text (unlines)
import           Data.Typeable   (Typeable)
import qualified Prelude         (error)
import           Protolude as P
import           Type.Reflection

-- | Container for a list of functions or values
--   Internally all functions and values are stored as Dynamic values
--   so that we can access their representation
data Registry (inputs :: [*]) (outputs :: [*]) =
  Registry {
    _overrides :: Overrides
  , _internal :: Internal inputs outputs
  }

-- List of types currently being built
newtype Context = Context [SomeTypeRep] deriving (Show)

-- List of values (either * or * -> *) available for constructing other values
newtype Constructors = Constructors [Dynamic] deriving (Show)

-- Specification of values which become available for
-- construction when a corresponding type comes in context
newtype Overrides = Overrides [(SomeTypeRep, Dynamic)] deriving (Show)

data Internal (inputs :: [*]) (outputs :: [*]) where
  RNil  :: Internal '[] '[]
  RCons :: (Typeable a) => a -> Internal ins out -> Internal (Union (Inputs a) ins) (Union '[Output a] out)

-- | Store an element in the registry
--   Internally elements are stored as dynamic values
register :: Typeable a => a -> Registry ins out -> Registry (Union (Inputs a) ins) (Union '[Output a] out)
register a (Registry c i) = Registry c (RCons a i)

-- | The empty Registry
end :: Registry '[] '[]
end = Registry (Overrides []) RNil

-- | Add an element to the Registry - Alternative to register where the parentheses can be ommitted
infixr 5 +:
(+:) :: Typeable a => a -> Registry ins out -> Registry (Union (Inputs a) ins) (Union '[Output a] out)
(+:) = register

specialize :: forall a b ins out . (Typeable a, Typeable b) => b -> Registry ins out -> Registry ins out
specialize b (Registry (Overrides c) internal) =
  Registry (Overrides ((someTypeRep (Proxy :: Proxy a), toDyn b) : c)) internal

-- | For a given registry make an element of type a
--   We want to ensure that a is indeed one of the return types
make :: forall a ins out . (Typeable a, Contains a out, Solvable ins out) => Registry ins out -> a
make = makeUnsafe

-- | This version of make only execute checks at runtime
--   this can speed-up compilation when writing tests or in ghci
makeUnsafe :: forall a ins out . (Typeable a) => Registry ins out -> a
makeUnsafe registry =
  let constructors = registryToConstructors registry
      overrides = _overrides registry
      targetType = someTypeRep (Proxy :: Proxy a)
  in
      -- | use the makeUntyped function to create an element of the target type from a list of constructors
      case makeUntyped targetType (Context [targetType]) overrides constructors of
        Nothing ->
          Prelude.error ("could not create a " <> show targetType <> " out of the registry")

        Just result ->
          case fromDynamic result of
            Nothing ->
              Prelude.error ("could not cast the computed value to a " <> show targetType <> ". The value is of type: " <> show (dynTypeRep result))

            Just other ->
              other

-- * Private - WARNING: HIGHLY UNTYPED IMPLEMENTATION !

-- | Return the registry as a list of constructors
registryToConstructors :: Registry ins out -> Constructors
registryToConstructors (Registry _ RNil)           = Constructors []
registryToConstructors (Registry c (RCons a rest)) =
  let Constructors cs = registryToConstructors (Registry c rest)
  in  Constructors (toDyn a : cs)

-- | Make a value from a desired output type represented by SomeTypeRep
--   and a list of possible constructors
--   A context is passed in the form of a stack of the types we are trying to build so far
makeUntyped ::
     SomeTypeRep
  -> Context
  -> Overrides
  -> Constructors
  -> Maybe Dynamic
makeUntyped targetType context overrides registry =

  -- is there already a value with the desired type?
  case findValue targetType context overrides registry of
    Nothing ->
      -- if not, is there a way to build such value?
     case findConstructor targetType registry of
        Nothing ->
          Nothing

        Just c ->
          applyFunction c <$> makeInputs (collectInputTypes c) context overrides registry

    other ->
      other

-- | If Dynamic is a function collect all its input types
collectInputTypes :: Dynamic -> [SomeTypeRep]
collectInputTypes = go . dynTypeRep
  where
    go :: SomeTypeRep -> [SomeTypeRep]
    go (SomeTypeRep (Fun in1 out)) = SomeTypeRep in1 : go (SomeTypeRep out)
    go _                           = []

-- | Apply a Dynamic function to a list of Dynamic values
applyFunction ::
     Dynamic    -- function
  -> [Dynamic]  -- inputs
  -> Dynamic    -- result
applyFunction f []     = Prelude.error $ "the function " <> show (dynTypeRep f) <> " cannot be applied to an empty list of parameters"
applyFunction f [i]    = dynApp f i
applyFunction f (i:is) = applyFunction (dynApp f i) is


-- | Find a value having a target type
--   from a list of dynamic values found in a list of constructors
--   where some of them are not functions
--   There is also a list of overrides when we can specialize the values to use
--   if a given type is part of the context
findValue ::
     SomeTypeRep
  -> Context
  -> Overrides
  -> Constructors
  -> Maybe Dynamic
-- no overrides or constructors to choose from
findValue _ _ (Overrides []) (Constructors []) = Nothing

-- recurse on the overrides first
findValue target (Context context) (Overrides ((t, v) : rest)) constructors =
  -- if there is an override which value matches the current target
  -- and if that override is in the current context then return the value
  if target == dynTypeRep v && t `elem` context then
    Just v
  else
    findValue target (Context context) (Overrides rest) constructors

-- otherwise recurse on the list of constructors until a value
-- with the target type is found
findValue target context overrides (Constructors (c : rest)) =
  case dynTypeRep c of

    -- if the current constructor is a function, skip it
    SomeTypeRep (Fun _ _) ->
      findValue target context overrides (Constructors rest)

    -- otherwise it is a value, take it if it is of the desired type
    other ->
      if other == target then
        Just c
      else
        -- otherwise recurse
        findValue target context overrides (Constructors rest)

-- | Find a constructor function returning a target type
--   from a list of constructorsfe
findConstructor ::
     SomeTypeRep
  -> Constructors
  -> Maybe Dynamic
findConstructor _ (Constructors []) = Nothing
findConstructor target (Constructors (c : rest)) =
  case dynTypeRep c of
    SomeTypeRep (Fun _ out) ->
      if outputType (SomeTypeRep out) == target then
        Just c
      else
        findConstructor target (Constructors rest)

    _ ->
      findConstructor target (Constructors rest)

-- | If the input type is a function type return its output type
outputType :: SomeTypeRep -> SomeTypeRep
outputType (SomeTypeRep (Fun _ out)) = outputType (SomeTypeRep out)
outputType r                         = r

-- | Make the input values of a given function
--   When a value has been made it is placed on top of the
--   existing registry so that it is memoized if needed in
--   subsequent calls
makeInputs ::
     [SomeTypeRep]   -- inputs to make
  -> Context         -- context
  -> Overrides       -- overrides
  -> Constructors    -- registry
  -> Maybe [Dynamic] -- list of made values
makeInputs ins (Context context) overrides constructors =
  reverse <$> go ins constructors Nothing
  where
    go ::
         [SomeTypeRep]   -- required input types
      -> Constructors    -- registry
      -> Maybe [Dynamic] -- made input values
      -> Maybe [Dynamic] -- result
    go [] _ res = res
    go (i : rest) (Constructors cs) made =
      if i `elem` context then
        Prelude.error $ toS $ unlines $
        ["cycle detected! The current types being built are "] <>
        (show <$> context) <>
        ["But we are trying to build again " <> show i]

      else
        case makeUntyped i (Context (i : context)) overrides (Constructors cs) of
          Nothing ->
            Nothing
          Just v ->
            go rest (Constructors (v : cs)) (((v :) <$> made) <|> Just [v])
