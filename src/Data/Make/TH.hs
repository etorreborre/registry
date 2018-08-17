{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

{-
  This module generates some haskell code for a Registry
  supporting Register instances for a set of types

   - a Registry datatype containing mn :: Maybe Tn members
     to store instances for input datatypes Tn

     data Registry = Registry {
       _t1 :: Maybe T1
     , _t2 :: Maybe T2
     }

   - a `defaultRegistry` function returning a `Registry` value
     where all members are set to Nothing

     defaultRegistry = Registry Nothing Nothing

   - a `Data.Default` instance for the Registry defined with `defaultRegistry`

   instance Default Registry where
    def = defaultRegistry

   - some Register Registry Tn instances for each input type
      instance Register Registry T1 where
        access = _t1
        register is m = is { _t1 = m }
-}

module Data.Make.TH where

import qualified Data.Char           as Char (toLower)
import           Data.Default
import           Data.Make.Register
import           Data.Text           (splitOn, toTitle)
import           Language.Haskell.TH
import           Prelude             (String)
import qualified Prelude             as Prelude (last)
import           Protolude           hiding (Type)

data RegistryOptions = RegistryOptions
  { _registryName          :: Text
  , _fieldNameFromTypeName :: Text -> Text
  }

-- | Create the haskell code presented in the box description
makeRegistry :: [Name] -> DecsQ
makeRegistry = makeRegistryWith (RegistryOptions "" ("_" <>))

makeRegistryWith :: RegistryOptions -> [Name] -> DecsQ
makeRegistryWith (RegistryOptions registryN fieldNameFromTypeName) names = do
  let registryName = mkName (toS registryN)
  let fields = fmap makeField names

  let boxDecl = [DataD [] registryName [] Nothing [RecC registryName fields] []]
  registryValue    <- makeDefaultRegistryValue registryName (length names)
  defaultRegistry  <- makeDefaultRegistry registryName
  registryInstances <- forM names (makeRegisterInstance fieldNameFromTypeName registryName)
  pure . join $
    [ boxDecl
    , registryValue
    , defaultRegistry
    , join registryInstances
    ]

  where
    -- make a record field for a given input type
    makeField n =
      ( makeFieldName fieldNameFromTypeName n
      , Bang NoSourceUnpackedness NoSourceStrictness
      , AppT (ConT ''Maybe) (ConT n)
      )

-- | Create a field name based on a fully qualified type name
makeFieldName :: (Text -> Text) -> Name -> Name
makeFieldName fieldNameFromTypeName n =
  mkName (toS . fieldNameFromTypeName . show $ n)

camelCaseTypeName :: Int -> Text -> Text
camelCaseTypeName n t = toS $
  case takeLast n (splitOn "." t) of
    []   -> ""
    [h]  -> uncapitalizeText h
    h:ts -> uncapitalizeText h <> toTitle (fold ts)

lastName :: Text -> Text
lastName t = Prelude.last $ splitOn "." t

takeLast :: Int -> [Text] -> [Text]
takeLast i = reverse . take i . reverse

uncapitalizeText :: Text -> Text
uncapitalizeText = toS . uncapitalize . toS

uncapitalize :: String -> String
uncapitalize []     = []
uncapitalize (h:ts) = Char.toLower h:ts

-- | Create a default value for Registry with all fields set to Nothing
--   The name of that function is registryDef
makeDefaultRegistryValue :: Name -> Int -> DecsQ
makeDefaultRegistryValue name n = do
  let valueName = mkName (uncapitalize (show name) <> "Def")
  sig <- sigD valueName (conT name)
  impl <- valD (varP valueName) (normalB (applyNothing n)) []
  pure [sig, impl]

  where
    -- default values are set to Nothing
    applyNothing 1 = appE (conE name) (conE 'Nothing)
    applyNothing i = appE (applyNothing (i - 1)) (conE 'Nothing)

-- | Create the Data.Default instances for Registry
makeDefaultRegistry :: Name -> DecsQ
makeDefaultRegistry registryName =
  let registryDefName = mkName (uncapitalize (show registryName) <> "Def")
  in  [d|instance Default $(conT registryName) where
           def = $(varE registryDefName) |]

-- | Make one Register Registry <type> instance
makeRegisterInstance :: (Text -> Text) -> Name -> Name -> DecsQ
makeRegisterInstance fieldNameFromTypeName registryName name = do
  let accessName = mkName "access"
  let registerName = mkName "register"
  ms <- newName "ms"
  m <- newName "m"

  let fieldName = makeFieldName fieldNameFromTypeName name
  pure [InstanceD Nothing [] (AppT (AppT (ConT ''Register) (ConT registryName))
    (ConT name))
      [ ValD (VarP accessName) (NormalB (VarE fieldName)) []
      , FunD registerName [Clause [VarP m, VarP ms]
            (NormalB (RecUpdE (VarE ms) [(fieldName, AppE (ConE 'Just) (VarE m))] )) []]
      ]
    ]
