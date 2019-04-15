{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Make.SpecializationSpec where

import           Data.Registry
import           Protolude             hiding (C1)
import           Test.Tasty.Extensions

-- | Case 1: contextual setting of different values for a given type
test_specialization_1 = test "values can use other values depending on some context" $ do
  (c1, c2) <- liftIO $
    do let r =    val (Config 3)
               +: fun newUseConfig1
               +: fun newUseConfig2
               +: end
       let r' = specialize @UseConfig1 (Config 1) $
                specialize @UseConfig2 (Config 2) r
       pure (printConfig1 (make @UseConfig1 r'), printConfig2 (make @UseConfig2 r'))

  c1 === Config 1
  c2 === Config 2

-- | Case 2: if there are 2 specialization taking effect for 2 different types
--   the one that is the children of the other in the current context wins
test_specialization_2 = test "more specialized context" $ do
  c <- liftIO $
    do let r =    val (Config 3)
               +: fun newUseConfig
               +: fun newClient1
               +: end
       let r' = specialize @Client1 (Config 1) $
                specialize @UseConfig (Config 2) r
       pure $ printClientConfig1 (make @Client1 r')

  annotate "this is the more specialized context"
  c === Config 2

-- | Case 3: this time the specialization must "propagate" to components
--   using the specialized values, note that the `UseConfig` component needs to be
--   duplicated because it is on the path of the specialization
test_specialization_3 = test "specialized values must be kept up to their start context" $ do
  (c1, c2) <- liftIO $
    do let r =    val (Config 3)
               +: fun newUseConfig
               +: fun newClient1
               +: fun newClient2
               +: fun newBase
               +: end
       let r' = specialize @Client1 (Config 1) $
                specialize @Client2 (Config 2) r
       pure $ printBase (make @Base r')

  c1 === Config 1
  c2 === Config 2


-- we want the following graph
{-
            +----------  Base  ------------+
            |                              |
            v                              v
   (client1 :: Client1)          (client2 :: Client2)
            |                              |
            v                              v
   (useConfig1 :: UseConfig) (useConfig2 :: UseConfig)
            |                              |
            v                              v
   (config1 :: Config)           (config2 :: Config)


-}

newtype Config = Config Int deriving (Eq, Show)

newtype UseConfig1 = UseConfig1 { printConfig1 :: Config }
newUseConfig1 config = UseConfig1 { printConfig1 = config }

newtype UseConfig2 = UseConfig2 { printConfig2 :: Config }
newUseConfig2 config = UseConfig2 { printConfig2 = config }

newtype UseConfig = UseConfig { printConfig :: Config }
newUseConfig config = UseConfig { printConfig = config }

newtype Client1 = Client1 { printClientConfig1 :: Config }
newClient1 useConfig = Client1 { printClientConfig1 = printConfig useConfig }

newtype Client2 = Client2 { printClientConfig2 :: Config }
newClient2 useConfig = Client2 { printClientConfig2 = printConfig useConfig }

newtype Base = Base { printBase :: (Config, Config) }
newBase client1 client2 = Base { printBase = (printClientConfig1 client1, printClientConfig2 client2) }

-- | Case 4: we can specialize values across a given "path" in the graph
test_specialization_4 = test "values can be specialized for a given path" $ do
  (c1, c2, c3) <- liftIO $
    do let r =    valTo @RIO (Config 3)
               +: funTo @RIO newUseConfig
               +: funTo @RIO newClient1
               +: funTo @RIO newClient2
               +: funTo @RIO newBase2
               +: end
       let r' = specializePathValTo @RIO @[RIO Base2, RIO Client1, RIO UseConfig] (Config 1) .
                specializeValTo @RIO @(RIO UseConfig) (Config 2) $ r

       printBase2 <$> (unsafeRun @Base2 r')

  c1 === Config 1
  c2 === Config 2
  c3 === Config 3

data Base2 = Base2 {
  client1   :: Client1
, useConfig :: UseConfig
, config3   :: Config
}
newBase2 = Base2

printBase2 Base2 {..} = (printClientConfig1 client1, printConfig useConfig, config3)

-- we want the following graph
{-
            +----------  Base2  -----------+-----> (config3 :: Config)
            |                              |
            v                              v
   (client1 :: Client1)        (useConfig2 :: UseConfig)
            |                              |
            v                              v
   (useConfig1 :: UseConfig)      (config2 :: Config)
            |
            v
   (config1 :: Config)
-}

-- | Case 5 (taken from a real case)
--   In that case a non-specialized value could be taken for a given
--   instead of being re-created because it has specialized dependencies for
--   a given context
--   For this test, we track how a component, the Supervisor, is being configured
--   depending on which path it belongs
test_specialization_5 = test "values can be specialized for a given path - other case" $ do
  let app = make @App appRegistry

  annotate "the stats store client is well configured"
  let (twitterConfig, statsSqlConfig, statsSupervisorConfig) = statsStoreConfig (statsStore app)
  twitterConfig         === "for the twitter client"
  statsSqlConfig        === "for sql under the stats store"
  statsSupervisorConfig === "default"

  annotate "the app is well configured"
  (app & supervisor & supervisorConfig) === ("default" :: Text)
  (app & sql & sqlConfig)               === ("for sql in general" :: Text)


appRegistry :: Registry _ _
appRegistry =
  specializeUnsafeVal @Sql (SupervisorConfig "for sql in general") .
  specializePathUnsafeVal @[StatsStore, Sql] (SupervisorConfig "for sql under the stats store") .
  specializeUnsafeVal @TwitterClient (SupervisorConfig "for the twitter client") $
     val (SupervisorConfig "default")
  +: fun newTwitterClient
  +: fun newSupervisor
  +: fun newStatsStore
  +: fun newSql
  +: fun App
  +: end

data App = App {
  sql           :: Sql
, twitterClient :: TwitterClient
, supervisor    :: Supervisor
, statsStore    :: StatsStore
}

newtype Sql = Sql { sqlConfig :: Text }
newtype StatsStore  = StatsStore { statsStoreConfig :: (Text, Text, Text) } -- (twitter, sql, supervisor)
newtype TwitterClient = TwitterClient { twitterConfig :: Text }
newtype Supervisor = Supervisor { supervisorConfig :: Text }
newtype SupervisorConfig = SupervisorConfig Text deriving (Eq, Show)

newSupervisor :: SupervisorConfig -> Supervisor
newSupervisor (SupervisorConfig n) = Supervisor { supervisorConfig = n }

newSql :: Supervisor -> Sql
newSql s = Sql { sqlConfig = supervisorConfig s }

newTwitterClient :: Supervisor -> TwitterClient
newTwitterClient s = TwitterClient { twitterConfig = supervisorConfig s }

newStatsStore :: TwitterClient -> Sql -> Supervisor -> StatsStore
newStatsStore client sql supervisor = StatsStore {
  statsStoreConfig = (twitterConfig client, sqlConfig sql, supervisorConfig supervisor)
}

-- | Case 6 (taken from a real case...)
test_specialization_6 = test "specialized values must not be affected by memoization" $ do
  someData <- liftIO $ do
    r <- aRegistryIO
    make @(IO SomeData) r

  (someData & toOverride & toOverrideConfig)       === ("specialized config" :: Text)
  (someData & toKeepDefault & toKeepDefaultConfig) === ("default config" :: Text)


data SomeData = SomeData {
  toKeepDefault :: ToKeepDefault
, toOverride    :: ToOverride
, inCommon      :: InCommon
}

newtype ToOverride = ToOverride { toOverrideConfig :: Text }
newtype ToKeepDefault = ToKeepDefault { toKeepDefaultConfig :: Text }
newtype InCommon = InCommon { config :: SomeConfig }

newtype SomeConfig = SomeConfig Text deriving (Eq, Show)

newToKeepDefault :: InCommon -> ToKeepDefault
newToKeepDefault (InCommon (SomeConfig t)) = ToKeepDefault { toKeepDefaultConfig = t }

newToOverride :: InCommon -> ToOverride
newToOverride (InCommon (SomeConfig t)) = ToOverride { toOverrideConfig = t }

aRegistryIO :: IO (Registry _ _)
aRegistryIO = memoizeAll @IO $
     specializePathUnsafeValTo @IO @[IO ToOverride, IO InCommon] (SomeConfig "specialized config") $
     valTo @IO (SomeConfig "default config")
  +: funTo @IO newToKeepDefault
  +: funTo @IO newToOverride
  +: funTo @IO InCommon
  +: funTo @IO SomeData
  +: end
