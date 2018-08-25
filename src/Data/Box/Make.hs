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
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

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
, end
, register
, (+:)
, make
) where

import           Data.Dynamic
import           Data.Typeable   (Typeable)
import qualified Prelude         (error)
import           Protolude
import           Type.Reflection

-- | Container for a list of functions or values
--   Internally all functions and values are stored as Dynamic values
--   so that we can access their representation
data Registry (els :: [*]) where
  RNil  :: Registry '[]
  RCons :: !Dynamic -> Registry els -> Registry (e ': els)

-- | Store an element in the registry
--   Internally elements are stored as dynamic values
register :: Typeable a => a -> Registry els -> Registry (a ': els)
register = RCons . toDyn

-- | The empty Registry
end :: Registry '[]
end = RNil

-- | Add an element to the Registry - Alternative to register where the parentheses can be ommitted
infixr 5 +:
(+:) :: Typeable a => a -> Registry els -> Registry (a ': els)
(+:) = register

class Contains (a :: *) (els :: [*])
instance {-# OVERLAPPING #-} Contains a (a ': els)
instance {-# OVERLAPPABLE #-} Contains a els => Contains a (b ': els)

-- * Private - WARNING: HIGHLY UNTYPED IMPLEMENATION !

-- | Return the registry as a list of constructors
registryToList :: Registry els -> [Dynamic]
registryToList RNil           = []
registryToList (RCons a rest) = a : registryToList rest

-- | For a given registry make an element of type a
make :: forall a . forall els . Typeable a => Registry els -> a
make registry =
  let constructors = registryToList registry
      targetType = someTypeRep (Proxy :: Proxy a)
  in
      -- | use the makeUntyped function to create an element of the target type from a list of constructors
      case makeUntyped targetType constructors of
        Nothing ->
          Prelude.error ("could not create a " <> show targetType <> " out of the registry")

        Just result ->
          case fromDynamic result of
            Nothing ->
              Prelude.error ("could not cast the computed value to a " <> show targetType <> ". The value is of type: " <> show (dynTypeRep result))

            Just other ->
              other

-- | Make a value from a desired output type represented by SomeTypeRep
--   and a list of possible constructors
makeUntyped :: SomeTypeRep -> [Dynamic] -> Maybe Dynamic
makeUntyped targetType registry =

  -- is there already a value with the desired type?
  case findValue targetType registry of
    Nothing ->
     -- if not, is there a way to build such value?
     case findConstructor targetType registry of
        Nothing ->
          Nothing

        Just c ->
          applyFunction c <$> (makeInputs (collectInputTypes c) registry)

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
--   from a list of dynamic values
findValue :: SomeTypeRep -> [Dynamic] -> Maybe Dynamic
findValue _ [] = Nothing
findValue target (c : rest) =
  case dynTypeRep c of
    SomeTypeRep (Fun _ _) ->
      Nothing

    other ->
      if other == target then
        Just c
      else
        findValue target rest

-- | Find a constructor function returning a target type
--   from a list of constructors
findConstructor :: SomeTypeRep -> [Dynamic] -> Maybe Dynamic
findConstructor _ [] = Nothing
findConstructor target (c : rest) =
  case dynTypeRep c of
    SomeTypeRep (Fun _ out) ->
      if outputType (SomeTypeRep out) == target then
        Just c
      else
        findConstructor target rest

    _ ->
      findConstructor target rest

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
  -> [Dynamic]       -- registry
  -> Maybe [Dynamic] -- list of made values
makeInputs ins r = reverse <$> go (Just []) ins r
  where
    go ::
         Maybe [Dynamic] -- result
      -> [SomeTypeRep]   -- required input types
      -> [Dynamic]       -- registry
      -> Maybe [Dynamic] -- made input values
    go Nothing _ _  = Nothing
    go res [] _ = res
    go (Just made) (i : rest) cs =
      case makeUntyped i cs of
        Nothing ->
          Nothing
        Just v ->
          go (Just $ v : made) rest (v : cs)
