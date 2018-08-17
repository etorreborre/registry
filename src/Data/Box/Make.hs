{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}


{-
  The Make typeclass describes a way to create a
  box using the current state of a Registry.

  The creation of a box might be effectful
  if some resources need to be allocated.

  The allocation is done with the `allocate` method
  which also specifies how the resources must be cleaned up.

  Creating a box can also return a warmup function
  to control that the box is correctly configured
  (see the Warmup box to learn how to create warmups)

-}
module Data.Box.Make (
  module MakeT
, TH.RegistryOptions
, Stop(..)
, Make(..)
, BoxRIO (..)
, allocate
, eval
, evalS
, liftRIO
, getInstance
, makeAll
, makeAllS
, makeBox
, makeRegistry
, makeRegistryWith
, makeForwarders
, runAll
, runAllS
, runStop
, startAll
, startAllS
, warmupWith
, withBox
, withBoxS
) where

import           Control.Monad.Trans.Resource as Resource hiding (allocate)
import qualified Control.Monad.Trans.Resource as Resource (allocate)
import           Data.Box.RIO
import           Data.Box.Warmup
import           Data.Make.Make               (getInstance)
import           Data.Make.MakeT              as MakeT
import           Data.Make.Registry
import qualified Data.Make.TH                 as TH
import           Language.Haskell.TH
import           Protolude

-- | Typeclass making and storing an object
--   This is essentially an alias to MakeT specialized to BoxRIO
class Make s a where
  -- | Produce a value of type a, from an initial state s, possibly with BoxRIO effects
  make :: StateT s BoxRIO a

-- | A registry with mandatory values and a value of type a belonging
--  to those mandatory values can make an instance of type a
instance (ExtractFromMand a mand) => Make (Registry opt mand) a where
  make = extractFromMand . _mandatory <$> get

-- | An instance which can be made without effects can
--   also be made in the BoxRIO monad
instance (Make s a) => MakeT s BoxRIO a where
  makeT = make

-- | Data type encapsulating resource finalizers
newtype Stop = Stop InternalState

-- | Independent function to run finalizers
runStop :: Stop -> RIO ()
runStop (Stop is) = runResourceT $ closeInternalState is

-- | This newtype creates a monad to sequence
--   box creation actions, cumulating start/stop tasks
--   found along the way
newtype BoxRIO a =
  BoxRIO
  { runBoxRIO :: Stop -> RIO (a, Warmup) }
  deriving (Functor)

instance Applicative BoxRIO where
  pure a =
    BoxRIO (const (pure (a, mempty)))

  BoxRIO fab <*> BoxRIO fa =
    BoxRIO $ \s ->
      do (f, sf) <- fab s
         (a, sa) <- fa s
         pure (f a, sf `mappend` sa)

instance Monad BoxRIO where
  return = pure

  BoxRIO ma >>= f =
    BoxRIO $ \s ->
      do (a, sa) <- ma s
         (b, sb) <- runBoxRIO (f a) s
         pure (b, sa `mappend` sb)

instance MonadIO BoxRIO where
  liftIO io = BoxRIO $ const (liftIO ((\a -> (a, mempty)) <$> io))

instance MonadResource BoxRIO where
  liftResourceT action = BoxRIO $ \(Stop s) -> liftIO ((, mempty) <$> runInternalState action s)

-- * For production

-- | Create a Registry for a list of types with TemplateHaskell
makeRegistry :: [Name] -> DecsQ
makeRegistry = makeRegistryWith (TH.camelCaseTypeName 2)

makeRegistryWith :: (Text -> Text) -> [Name] -> DecsQ
makeRegistryWith fieldNameFromTypeName = TH.makeRegistryWith (TH.RegistryOptions "Box" fieldNameFromTypeName)

-- | Make some "Make" typeclass forwarders from a field of Instances
--   to the fields of another data structure
--   For example if Instances has a 'config :: Config' field
---  where Config is data Config = Config { _m1 :: Int, _m2 :: Text }
--   this will generate
--      instance Make Box Config where
--         make = fmap _m1 make
--      instance Make Box Config where
--         make = fmap _m2 make
makeForwarders :: Name -> Name -> DecsQ
makeForwarders boxesName forwardToName = do
   TyConI (DataD _cxt _name _tyVar _kind [RecC _ fields] []) <- reify forwardToName
   let fieldNames = (\(fName, _, ConT fTypeName) -> (fName, fTypeName)) <$> fields
   forM fieldNames (makeForwarderInstance boxesName)

makeForwarderInstance :: Name -> (Name, Name) -> Q Dec
makeForwarderInstance boxesName (fieldName, fieldTypeName) = pure $
  InstanceD Nothing [] (AppT (AppT (ConT (mkName "Make")) (ConT boxesName)) (ConT fieldTypeName))
      [ ValD (VarP (mkName "make")) (NormalB (AppE (AppE (VarE (mkName "fmap")) (VarE fieldName)) (VarE (mkName "make")))) []]


-- | This function must be used to run services involving a top module
--   It creates the top box and invokes all warmup functions
--
--   The passed function 'f' is used to decide whether to continue or
--   not depending on the Result
withBox :: Make s m =>
     s
  -> (Result -> m -> RIO a)
  -> RIO a
withBox = withBoxS make

withBoxS ::
      StateT s BoxRIO m
  ->  s
  -> (Result -> m -> RIO a)
  -> RIO a
withBoxS st s f = runResourceT $ do
  (m, r) <- runAllS st s
  lift $ f r m

-- * For testing / exploration

-- | Start a box and have all the warmup actions executed
--   but their result discarded, meaning that one warmup might
--   fail but we will still continue
startAll :: Make s m => s -> ResourceT RIO m
startAll = startAllS make

startAllS :: StateT s BoxRIO m -> s -> ResourceT RIO m
startAllS st s = fst <$> runAllS st s

-- | Start a box and have all the warmup actions executed sequentially,
--   bottom up
runAll :: Make s m => s -> ResourceT RIO (m, Result)
runAll = runAllS make

runAllS :: StateT s BoxRIO m -> s -> ResourceT RIO (m, Result)
runAllS st s = do
  (m, warmup) <- makeAllS st s
  r <- lift $ runWarmup warmup
  pure (m, r)

-- | Construct a box (and all its dependencies)
makeAll :: Make s m => s -> ResourceT RIO (m, Warmup)
makeAll = makeAllS make

makeAllS :: StateT s BoxRIO m -> s -> ResourceT RIO (m, Warmup)
makeAllS st s = withInternalState $ \is ->
  runBoxRIO (evalStateT st s) (Stop is)

-- | Construct a box (and all its dependencies)
--   Do not warmup or cleanup on exit
eval :: Make s m => s -> RIO (m, Warmup, Stop)
eval = evalS make

evalS :: StateT s BoxRIO m -> s -> RIO (m, Warmup, Stop)
evalS st s =
  do is <- createInternalState
     (m, w) <- runBoxRIO (evalStateT st s) (Stop is)
     pure (m, w, Stop is)

makeBox :: Make () m => RIO (m, Warmup, Stop)
makeBox = eval ()

-- * Box creation

warmupWith :: Warmup -> BoxRIO ()
warmupWith w = BoxRIO (const $ pure ((), w))

allocate :: RIO a -> (a -> RIO ()) -> BoxRIO a
allocate resource cleanup =
  snd <$> Resource.allocate (run resource) (run . cleanup)

liftRIO :: RIO a -> BoxRIO a
liftRIO rio = BoxRIO (\_ -> (, mempty) <$> rio)
