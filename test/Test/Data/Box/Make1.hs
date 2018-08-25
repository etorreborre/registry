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

module Test.Data.Box.Make1 where

import           Data.Text     as T (length)
import           Data.Typeable  (Typeable)
import qualified Prelude       as Prelude (last, show, error)
import Data.Maybe (fromJust)
import           Protolude hiding (typeRep)
import Data.Dynamic
import Type.Reflection

-- * Registry as a HList of dynamic values

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

infixr 5 +:
(+:) :: Typeable a => a -> Registry els -> Registry (a ': els)
(+:) = register

class Contains (a :: *) (els :: [*])
instance {-# OVERLAPPING #-} Contains a (a ': els)
instance {-# OVERLAPPABLE #-} Contains a els => Contains a (b ': els)

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
            fromJust $ fromDynamic result

-- | Make a value from a desired output type represented by SomeTypeRep
--   and a list of possible constructors
makeUntyped :: SomeTypeRep -> [Dynamic] -> Maybe Dynamic
makeUntyped targetType registry =
  trace' ("trying to make a value of type  " <> show targetType) $
  case findValue targetType registry of
    Just v -> trace' ("found a value" <> show v) $ Just v
    Nothing ->
      trace' ("no value for the target type, trying to find a constructor") $
      case findConstructor targetType registry of
        Nothing ->
          trace' ("no constructor found") $
          Nothing
        Just c ->
          traceShow "found a constructor" $
          let madeInputs = makeInputs (collectInputs c) registry
          in  applyFunction c <$> madeInputs

collectInputs :: Dynamic -> [SomeTypeRep]
collectInputs = go . dynTypeRep
  where
    go :: SomeTypeRep -> [SomeTypeRep]
    go (SomeTypeRep (Fun in1 out)) = SomeTypeRep in1 : go (SomeTypeRep out)
    go _ = []


applyFunction ::
     Dynamic    -- function
  -> [Dynamic]  -- inputs
  -> Dynamic    -- result
applyFunction f [i]    = dynApp f i
applyFunction f (i:is) = applyFunction (dynApp f i) is


-- | Find a value having a target type
--   from a list of dynamic values
findValue :: SomeTypeRep -> [Dynamic] -> Maybe Dynamic
findValue _ [] = Nothing
findValue target (c : rest) =
  case dynTypeRep c of
    SomeTypeRep (Fun _ out) ->
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

outputType :: SomeTypeRep -> SomeTypeRep
outputType (SomeTypeRep (Fun _ out)) = outputType (SomeTypeRep out)
outputType r = r

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
          trace' ("made a value " <> show v) $
          go (Just $ v : made) rest (v : cs)

trace' :: Text -> a -> a
trace' = trace

-- | Examples
newtype Config1 = Config1 { c1 :: Int }
newtype Config2 = Config2 { c2 :: Int }

data Log = Log { run :: Text -> IO () }

newLog :: Config1 -> Config2 -> IO Log
newLog _ _ = pure (Log (const $ pure ()))

int1 :: Int
int1 = 1

add1 :: Int -> Text
add1 i = show (i + 1)

newtype Text1 = Text1 Text deriving (Eq, Show, Typeable)
newtype Text2 = Text2 Text deriving (Eq, Show, Typeable)
newtype Int1 = Int1 Int deriving (Eq, Show, Typeable)

add2 :: Int -> Text -> Text1
add2 i j = Text1 (show (i+1) <> j)

text1 :: Text
text1 = "text1"

toText2 :: Text1 -> Text2
toText2 (Text1 t) = (Text2 t)

registry1 :: Registry [Int, Int -> Text, Int -> Text -> Text1, Text1 -> Text2]
registry1 =
     int1
  +: add1
  +: add2
  +: toText2
  +: end

registry2 =
     int1
  +: add1
  +: countSize1
  +: end

countSize :: Text -> Maybe Int
countSize t = Just (T.length t)

countSize1 :: Text -> Int1
countSize1 t = Int1 (T.length t)

made1 :: Text
made1 = make @Text registry1

made2 :: Text1
made2 = make @Text1 registry1

made3 :: Int1
made3 = make @Int1 registry2

made4 :: Text2
made4 = make @Text2 registry1
