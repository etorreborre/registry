{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Box.App where

import           Data.Box.Make
import           Data.Box.RIO
import           Data.Box.Warmup
import           Protolude                     as P
import qualified Test.Data.Box.Authentication  as Authentication
import qualified Test.Data.Box.EventProcessing as EventProcessing
import qualified Test.Data.Box.Logging         as Logging
import qualified Test.Data.Box.MasterData      as MasterData
import           Test.Data.Box.Uri

main :: IO ()
main = pure ()

makeRegistry
  [ ''Logging.Config
  , ''Authentication.Config
  , ''MasterData.Config
  , ''EventProcessing.Config
  , ''Logging.Box
  , ''Authentication.Box
  , ''MasterData.Box
  , ''EventProcessing.Box
  ]

prodAuthenticationConfig :: Authentication.Config
prodAuthenticationConfig =
  Authentication.Config
    (Uri "authentication-uri")

prodMasterDataConfig :: MasterData.Config
prodMasterDataConfig =
  MasterData.Config
    (Uri "master-data-uri")

prodEventProcessingConfig :: EventProcessing.Config
prodEventProcessingConfig =
  EventProcessing.Config
    (Uri "event-subscription")

noLogging =
  Logging.noLogging

prodBox :: Box
prodBox = boxDef

silentBox :: Box
silentBox =
  prodBox
  { loggingBox         = Just noLogging
  , authenticationBox  = Just Authentication.ok
  , loggingConfig         = Just (Logging.Config P.print)
  , authenticationConfig  = Just prodAuthenticationConfig
  , masterDataConfig      = Just prodMasterDataConfig
  , eventProcessingConfig = Just prodEventProcessingConfig
  }

g :: (Make Box EventProcessing.Config) => StateT Box BoxRIO EventProcessing.Config
g = make

startApp :: (Make Box m) => Box -> (m -> RIO ()) -> RIO ()
startApp ms f = withBox ms $ \r m ->
  if isSuccess r then
    do liftIO $ print ("box started ok " <> show r :: Text)
       f m
  else
    liftIO $ print ("could not properly start box, aborted " <> show r :: Text)

instance Make Box EventProcessing.Config where
  make = pure (EventProcessing.Config (Uri "http://localhost:8080"))

instance Make Box MasterData.Config where
  make = pure (MasterData.Config (Uri "http://localhost:9090"))

instance Make Box Authentication.Config where
  make = pure (Authentication.Config (Uri "http://localhost:9090"))

instance Make Box Logging.Config where
  make = pure (Logging.Config P.print)


processEventsProd =
  startApp prodBox EventProcessing.processEvents

processEventsSilently =
  startApp silentBox EventProcessing.processEvents
