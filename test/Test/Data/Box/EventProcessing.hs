{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Data.Box.EventProcessing (
  Box
, Config(..)
, new
, processEvents
) where

import           Data.Box.Make
import           Data.Box.RIO
import           Prelude                  (show)
import           Protolude
import           Test.Data.Box.Logging    (logInfo)
import qualified Test.Data.Box.Logging    as Logging
import           Test.Data.Box.MasterData (getAttributes)
import qualified Test.Data.Box.MasterData as MasterData
import           Test.Data.Box.Uri

newtype Box =
  Box
  { processEvents :: RIO ()
  }

instance Show Box where
  show _ = "event processing box"

newtype Config =
  Config
  { uri :: Uri
  } deriving (Show)

instance ( Register s Box
         , Make s Config
         , Make s MasterData.Box
         , Make s Logging.Box
         ) => Make s Box where
  make = create3 new

new :: Config -> MasterData.Box -> Logging.Box -> BoxRIO Box
new (Config uri1) masterData logging =
  pure $ Box $ (processEventsWith masterData logging) uri1

processEventsWith :: MasterData.Box -> Logging.Box -> Uri -> RIO ()
processEventsWith masterData logging _uri = do
  logging & logInfo $ ("hey" :: Text)
  _attributes <- masterData & getAttributes
  pure ()
