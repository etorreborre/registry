{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Data.Box.MasterData (
  Box
, Config(..)
, getAttributes
, new
) where

import           Data.Box.Make
import           Data.Box.RIO
import           Data.Box.Warmup
import           Prelude                      (show)
import           Protolude
import qualified Test.Data.Box.Authentication as Authentication
import           Test.Data.Box.Logging        (logInfo)
import qualified Test.Data.Box.Logging        as Logging
import           Test.Data.Box.Uri

data Box =
  Box
  { getAttributes :: RIO [Text]
  }

instance Show Box where
  show _ = "master data box"

newtype Config =
  Config {
    uri :: Uri
  } deriving (Show)

instance ( Register s Box
         , Make s Config
         , Make s Logging.Box
         , Make s Authentication.Box
         ) => Make s Box where
  make = create3 new

checkAttributes :: Box -> Warmup
checkAttributes m = createWarmup $ do
  attributes <- m & getAttributes
  pure $
    if null attributes then
      failed "there must be attributes"
    else
      ok "started master data"

new :: Config -> Logging.Box -> Authentication.Box -> BoxRIO Box
new (Config u) logging authentication =
  let
    authenticate = Authentication.authenticate authentication
  in do let m = Box (getAttributesWith logging u authenticate)
        warmupWith (checkAttributes m)
        pure m

getAttributesWith :: Logging.Box -> Uri -> RIO () -> RIO [Text]
getAttributesWith logging (Uri u) authenticate = do
  logging & logInfo $ ("doing some master data stuff with " <> u)
  authenticate
  pure []
