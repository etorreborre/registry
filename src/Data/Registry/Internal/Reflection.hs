{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeInType          #-}

{-
  Utility functions to display types
-}
module Data.Registry.Internal.Reflection where

import           Data.Semigroup
import           Data.Text as T
import           Protolude       as P hiding (intercalate, TypeRep, isPrefixOf, (<>))
import           Type.Reflection
import           GHC.Exts

-- | Show a value with its type first
showValue :: (Typeable a, Show a) => a -> Text
showValue a = showFullType a <> ": " <> show a

-- | Show a function (which doesn't have a Show instance)
showFunction :: Typeable a => a -> Text
showFunction = showFullType

-- | Show the full type of a typeable value
showFullType :: Typeable a => a -> Text
showFullType = showTheFullType . typeOf

-- | Show the full type of a typeable value
--   where nested types like IO[Int] or functions are represented and
--   non GHC types are shown with their module names
showTheFullType :: forall (r1 :: RuntimeRep) (arg :: TYPE r1) . (TypeRep arg -> Text)
showTheFullType a =
  case a of
    Fun t1 t2 ->
      showTheFullType t1 <> " -> " <> showTheFullType t2

    Fun (App t1 t2) t3 ->
      showNested (SomeTypeRep t1) (SomeTypeRep t2) <> " -> " <> showTheFullType t3

    App t1 t2 ->
      showNested (SomeTypeRep t1) (SomeTypeRep t2)

    _ ->
      showSingleType (SomeTypeRep a)

-- | Show a type like m a
showNested :: SomeTypeRep -> SomeTypeRep -> Text
showNested a b =
  parenthesizeNested $ tweakNested $ showSingleType a <> " " <> showSingleType b

-- | Show a single type. Don't display the module for GHC types
showSingleType :: SomeTypeRep -> Text
showSingleType a =
  let withModuleName = showWithModuleName a
  in  if mustShowModuleName withModuleName
      then withModuleName
      else show a

-- | Return true if the module name can be shown
mustShowModuleName :: Text -> Bool
mustShowModuleName name = not $ P.any identity $
  fmap (`isPrefixOf` name) [
      "GHC.Types."    -- for Int, Double,..
    , "GHC.Base."     -- for Maybe
    , "Data.Either."  -- for Either
    , "Data.Text.Internal"]

-- | Tweak some standard module names for better display
tweakNested :: Text -> Text
tweakNested "[] Char" = "String"
tweakNested n =
  if "[] " `isPrefixOf` n then
    "[" <> T.drop 3 n <> "]" -- special processing for lists
  else
    n

parenthesizeNested :: Text -> Text
parenthesizeNested t =
  case T.splitOn " " t of
    [] -> t
    [_] -> t
    [outer, inner] -> outer <> " " <> inner
    outer : rest -> outer <> " (" <> parenthesizeNested (T.intercalate " " rest) <> ")"

-- | Show a type with its module name
showWithModuleName :: SomeTypeRep -> Text
showWithModuleName t = (toS . tyConModule . someTypeRepTyCon $ t) <> "." <> show t
