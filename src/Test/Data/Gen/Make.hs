{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
  This module helps with the creation of Hedgehog
  generators for nested data so that we can have:

   - a set of default generators
   - a way to modify any generator in the "tree"
     of all the generators
-}
module Test.Data.Gen.Make (
  module TH
, module Make
, makeGens
) where

import           Data.Make.Make      as Make
import           Data.Make.TH        as TH
import           Hedgehog
import           Language.Haskell.TH
import           Protolude

makeGens :: [Name] -> DecsQ
makeGens names = do
  let typeNames = (\n -> "Gen" <> lastName (show n)) <$> names
  typeAliases <- sequence $ join $ forM names $ \tn ->
                   let typeAlias = mkName (toS $ "Gen"<>lastName (show tn))
                   in [tySynD typeAlias [] (appT (conT (mkName "Gen")) (conT tn))]

  gensData <- makeRegistryWith (RegistryOptions "Gens" (camelCaseTypeName 1)) (mkName . toS <$> typeNames)

  let gensName = mkName "Gens"
  genFunction <-
    [d|gen :: Make $(conT gensName) (Gen a) => (Gen a)
       gen = $(appTypeE (varE (mkName "getInstance")) (conT gensName))
       |]

  makeGenFunction <-
    [d|makeGen :: Make $(conT gensName) (Gen a) => $(conT gensName) -> Gen a
       makeGen = $(appTypeE (varE (mkName "makeInstance")) (conT gensName))
    |]

  evalGenFunction <-
    [d|evalGen :: Make $(conT gensName) (Gen a) => State $(conT gensName) $(conT gensName) -> Gen a
       evalGen _ss = $(appE (varE (mkName "makeGen")) (appE (appE (varE (mkName "evalState")) (varE (mkName "_ss"))) (varE (mkName "gensDef"))))
    |]

  pure $
    typeAliases ++
    gensData ++
    genFunction ++
    makeGenFunction ++
    evalGenFunction
