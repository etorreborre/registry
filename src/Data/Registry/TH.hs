{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Registry.TH (
  TypeclassOptions
, checkRegistry
, makeTypeclass
, makeTypeclassWith
, unsafeCoerceRegistry
) where

import           Data.List                  (nubBy)
import           Data.Registry
import           Data.Set                   (difference)
import qualified Data.Set                   as Set
import           Data.Text                  as T (drop, splitOn)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Prelude                    (String)
import           Protolude                  hiding (Strict, Type)

{-
  This module generates a typeclass for a given "record of functions". For this component:

data Logger m = Logger {
  _info :: Text -> m ()
, _error :: Text -> m ()
}

-- `makeTypeClass ''Logger` generates

class WithLogger m where
  info :: Text -> m ()
  error :: Text -> m ()

-- This requires the import of `Data.Generics.Product.Typed` from `generic-lens`
instance HasType (Logger m) s => WithLogger (ReaderT s m) where
  info t = ReaderT (\l -> _info (getTyped l) t)
  error t = ReaderT (\l -> _error (getType l) t)

-}

-- | Create the haskell code presented in the module description
makeTypeclass :: Name -> DecsQ
makeTypeclass = makeTypeclassWith (TypeclassOptions ("With" <>) (T.drop 1))

-- | These generation options can be used to tweak the generated names
data TypeclassOptions = TypeclassOptions {
  -- adjust the typeclass name based on the component constructor name
  _typeclassName :: Text -> Text
  -- adjust the typeclass function names based on the component function names
, _functionName  :: Text -> Text
}

-- | Make a typeclass using some specific generation options
makeTypeclassWith :: TypeclassOptions -> Name -> DecsQ
makeTypeclassWith (TypeclassOptions typeclassNameMaker functionNameMaker) componentType = do
  info <- reify componentType
  case info of
    TyConI (DataD _ name typeVars _ [RecC _ types] _) -> do
      readertInstance <- createReadertInstance typeclassNameMaker functionNameMaker name typeVars types
      pure $ createTypeclass typeclassNameMaker functionNameMaker name typeVars types
             <> readertInstance

    TyConI (NewtypeD _ name typeVars _ (RecC _ types) _) -> do
      readertInstance <- createReadertInstance typeclassNameMaker functionNameMaker name typeVars types
      pure $ createTypeclass typeclassNameMaker functionNameMaker name typeVars types
             <> readertInstance
    other -> do
      qReport True ("can only generate a typeclass for a record of functions, got: " <> show other)
      pure []


createTypeclass :: (Text -> Text) -> (Text -> Text) -> Name -> [TyVarBndr] -> [VarBangType] -> [Dec]
createTypeclass typeclassNameMaker functionNameMaker name typeVars types =
  let typeclassName = modifyName typeclassNameMaker (dropQualified name)
      functions = fmap (makeFunctionDeclaration functionNameMaker) types
  in [ClassD [] typeclassName typeVars [] functions]

-- | Create an instance definition using a ReaderT instance
--     instance WithLogger (ReaderT (Logger m) m) where ...
createReadertInstance :: (Text -> Text) -> (Text -> Text) -> Name -> [TyVarBndr] -> [VarBangType] -> DecsQ
createReadertInstance typeclassNameMaker functionNameMaker name [tvar] types =
  let tvarName = case tvar of PlainTV v -> v; KindedTV v _ -> v
      typeclassName = modifyName typeclassNameMaker (dropQualified name)
      functions = fmap (makeFunctionInstance functionNameMaker (mkName "ReaderT")) types
      typeclassT = ConT typeclassName
      components = mkName "c"
      componentTypeT = ConT name
      componentsTypeT = VarT components
      readerT = ConT (mkName "ReaderT")
      hasTypeT = ConT (mkName "HasType")
      tvarT   = VarT tvarName
  in pure [InstanceD Nothing
            [AppT (AppT hasTypeT (AppT componentTypeT tvarT)) componentsTypeT]
            (AppT typeclassT (AppT (AppT readerT componentsTypeT) tvarT))
            functions]

createReadertInstance _ _ _ tvars _ = do
  qReport True ("can only generate a instance for a component typeclass when it has only one type variable, got: " <> show tvars)
  pure []

-- | Make the function declaration of the typeclass based on the function name in the "record of functions"
makeFunctionDeclaration :: (Text -> Text) -> VarBangType -> Dec
makeFunctionDeclaration functionNameMaker (name, _, type') =
  SigD (modifyName functionNameMaker (dropQualified name)) type'

-- | This produces: info p1 p2 = ReaderT (\component -> _info component p1 p2)
makeFunctionInstance :: (Text -> Text) -> Name -> VarBangType -> Dec
makeFunctionInstance functionNameMaker runnerName (name, _, functionType) =
  let functionName = modifyName functionNameMaker (dropQualified name)
      readerT = ConE runnerName
      component = mkName "component"
      numberOfParameters = countNumberOfParameters functionType
      parameterNames = (\i -> mkName ("p" <> show i)) <$> [1..numberOfParameters]
      parameters = VarP <$> parameterNames
      firstApplication = AppE (VarE name) (AppE (VarE (mkName "getTyped")) (VarE component))
      body = foldl' (\r p -> AppE r (VarE p)) firstApplication parameterNames
  in
    FunD functionName [Clause parameters (NormalB (AppE readerT (LamE [VarP component] body))) []]

-- | count the number of parameters for a function type
countNumberOfParameters :: Type -> Int
countNumberOfParameters (ForallT _ _ t)          = countNumberOfParameters t
countNumberOfParameters (AppT (AppT ArrowT _) t) = 1 +  countNumberOfParameters t
countNumberOfParameters _                        = 0

-- | Modify a template haskell name
modifyName :: (Text -> Text) -> Name -> Name
modifyName f n = mkName (toS . f . show $ n)

-- | Remove the module name from a qualified name
dropQualified :: Name -> Name
dropQualified name =  maybe name (mkName . toS) (lastMay (T.splitOn "." (show name)))

-- | Check that all the input values of a registry can be built
--   This will check that all the input values can be built out of the registry
--   and also return a normalized registry where the types have been de-duplicated
--
--   Usage:
--
--     initialRegistry :: Registry _ _
--     initialRegistry = val x +: fun y +: ... +: end
--
--     -- Put the definition in another module! (see: https://gitlab.haskell.org/ghc/ghc/issues/9813)
--
--     checkedRegistry :: Registry _ _
--     checkedRegistry = $(checkRegistry initialRegistry)
--
checkRegistry :: Name -> Q Exp
checkRegistry registryName = do
  registryInfo <- reify registryName

  case registryInfo of

    VarI _ registryType _ ->
      case registryType of
        AppT (AppT (ConT actualType) ins) out -> do
          let actual = show actualType :: String
          if actual == "Data.Registry.Registry.Registry" then do

            let insTypes = fst <$> typesOf ins
            let outTypes = fst <$> typesOf out
            let missingFromOutputs = Set.fromList insTypes `difference` Set.fromList outTypes

            -- We check that all the input types to functions can be created
            -- from outputs in the registry
            if null missingFromOutputs then
              [| unsafeCoerceRegistry $(varE registryName) :: $(returnQ $ AppT (AppT (ConT actualType) (normalizeTypes ins)) (normalizeTypes out)) |]
            else
              reportErrorWith $ "Some input values cannot be built from the registry. " <> show (Set.toList missingFromOutputs)

          else
            reportErrorWith $ "We can only check the coverage of a Registry, got: " <> actual

        _ ->
          reportErrorWith $ "We can only check the coverage of a Registry. Use `checked = $(checkRegistry 'registry), Got: " <> show registryType

    other ->
      reportErrorWith $ "We can only check the coverage of a Registry. Use `checked = $(checkRegistry 'registry). Got: " <> show other

   where reportErrorWith msg = do
           reportError msg
           varE registryName


-- | Return a list of type name + type from a type level list of types
typesOf :: Type -> [(String, Type)]
typesOf (AppT (AppT PromotedConsT t) rest) = (typeName t, t) : typesOf rest
typesOf _ = []

-- | Extract the name of a type, prettified up to 2 type constructors
typeName :: Type -> String
typeName (ConT n) = nameBase n
typeName (AppT (ConT t1) (ConT t2)) = nameBase t1 <> "[" <> nameBase t2 <> "]"
typeName (AppT (AppT (ConT t1) (ConT t2)) (ConT t3)) = nameBase t1 <> "[" <> nameBase t2 <> "[" <> nameBase t3 <> "]" <> "]"
typeName t = show t

-- | Return a deduplicated list of types from a list of types
normalizeTypes :: Type -> Type
normalizeTypes t =
  rebuild $ nubBy (\(n1, _) (n2, _) -> n1 == n2) (typesOf t)
  where rebuild []               = SigT PromotedNilT (AppT ListT StarT)
        rebuild ((_, t1) : rest) = AppT (AppT PromotedConsT t1) (rebuild rest)

-- | This is unsafe and is only used in the context of the checkRegistry function
unsafeCoerceRegistry :: Registry ins out -> Registry ins1 out1
unsafeCoerceRegistry (Registry a b c d) = Registry a b c d
