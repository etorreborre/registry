{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Test.Data.Gen.THSpec where

import           Data.Make.Make
import           Data.Make.TH
import           Protolude      hiding (M1)

-- * Test for creating a registry with template haskell
data M1 = M1
data M2 = M2
data M3 = M3
data M4 = M4

makeRegistryWith (RegistryOptions "Mods" (("_" <>) . camelCaseTypeName 1)) [''M1, ''M2, ''M3, ''M4]
