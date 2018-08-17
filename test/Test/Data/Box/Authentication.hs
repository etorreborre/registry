{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Data.Box.Authentication (
  Box(..)
, Config(..)
, new
, ok
) where

import           Data.Box.Make
import           Data.Box.RIO
import           Prelude               (show)
import           Protolude             as P
import           Test.Data.Box.Logging (logInfo)
import qualified Test.Data.Box.Logging as Logging
import           Test.Data.Box.Uri
import           Universum             ((...))

data Box =
  Box
  { authenticate :: RIO ()
  }

instance ( Register s Box
         , Make s Config
         , Make s Logging.Box) => Make s Box where
  make = create2 (pure ... new)

instance Show Box where
  show _ = "authentication box"

newtype Config =
  Config
  { _uri :: Uri
  } deriving (Show)

new :: Config -> Logging.Box -> Box
new _ logging = Box (authenticateWith logging)

ok :: Box
ok = Box (print ("always OK" :: Text))

authenticateWith :: Logging.Box -> RIO ()
authenticateWith logging = do
  logging & logInfo $ ("authenticate" :: Text)
  print ("authenticating with flow id " :: Text)
