{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Box.MakeWithRegistrySpec where

import           Data.Box.Make
import           Data.Box.RIO
import           Data.Make.Registry
import           Protolude          hiding (C1)

newtype A0 = A0 Int
newtype A1 = A1 Int
newtype A2 = A2 Int
newtype A3 = A3 Int

printA0 :: A0 -> RIO ()
printA0 (A0 i) = liftIO (print i)

instance (Register s A0, Make s C1) => Make s A0 where
  make = pure (A0 0)

newtype C1 = C1 Int
newtype C2 = C2 Int
newtype C3 = C3 Int


appRegistry :: Registry '[A0, A1, A2] '[C1, C2, C3]
appRegistry =
     C1 1
  +: C2 1
  +: C3 1
  +: registry

theConfig1 :: C1
theConfig1 = extractFrom appRegistry

theConfig2 :: C2
theConfig2 = extractFrom appRegistry

theConfig3 :: C3
theConfig3 = extractFrom appRegistry

runIt = withBox appRegistry $ \_ a0 ->
  printA0 (a0 :: A0)
