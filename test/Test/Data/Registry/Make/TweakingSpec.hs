{-# LANGUAGE DataKinds        #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Make.TweakingSpec where

import           Data.Registry
import           Protolude
import           Test.Tasty.Extensions

-- | Modification of stored values
test_tweak = test "created values can be modified prior to being stored" $ do
  c1 <- liftIO $
    do let r =    val (Config 1)
               +: fun newUseConfig1
               +: fun newAppUsingConfig1
               +: end
       let r' = tweak (\(UseConfig1 _) -> UseConfig1 (Config 10)) r
       pure (printAppConfig (make @AppUsingConfig1 r'))

  c1 === Config 10

newtype AppUsingConfig1 = AppUsingConfig1  { printAppConfig :: Config }
newAppUsingConfig1 config1 = AppUsingConfig1  { printAppConfig = printConfig1 config1 }

newtype Config = Config Int deriving (Eq, Show)

newtype UseConfig1 = UseConfig1 { printConfig1 :: Config }
newUseConfig1 config = UseConfig1 { printConfig1 = config }
