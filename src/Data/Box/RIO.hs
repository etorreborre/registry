{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-

  The RIO monad is a Reader monad on top of IO
  It uses a simple environment with a single Aeson value
  which can (dynamically) represent any structure

-}
module Data.Box.RIO where

import           Control.Monad.Catch
import qualified Control.Monad.Catch     as Catch
import           Control.Monad.IO.Unlift (MonadUnliftIO, UnliftIO (..),
                                          askUnliftIO)
import           Data.Aeson
import           Protolude               hiding (catch, mask,
                                          uninterruptibleMask)

newtype RIO a = RIO { runRIO :: Env -> IO a }

instance Functor RIO where
  fmap f rio = RIO (fmap f . runRIO rio)

instance Applicative RIO where
  pure a = RIO (const (pure a))
  a <*> b = RIO (\r -> runRIO a r <*> runRIO b r)

instance Monad RIO where
  rio >>= f = RIO (\r -> runRIO rio r >>= (\a -> runRIO (f a) r))

instance MonadIO RIO where
  liftIO = RIO . const

instance MonadThrow RIO where
  throwM = liftIO . throwM

instance MonadCatch RIO where
  catch a handler =
    RIO $ \r -> catch (execRIO r a) (\e -> execRIO r $ handler e)

instance MonadMask RIO where
  mask a = RIO $ \e -> Catch.mask $ \u -> runRIO (a $ q u) e
    where q :: (IO a -> IO a) -> RIO a -> RIO a
          q u (RIO b) = RIO (u . b)

  uninterruptibleMask a =
    RIO $ \e -> Catch.uninterruptibleMask $ \u -> runRIO (a $ q u) e
      where q :: (IO a -> IO a) -> RIO a -> RIO a
            q u (RIO b) = RIO (u . b)

  generalBracket acquire release use = RIO $ \env ->
    mask $ \unmasked ->
      do a <- execRIO env acquire
         b <- unmasked (execRIO env (use a)) `catch` \e ->
                execRIO env (release a (ExitCaseException e)) >> throwM e
         c <- execRIO env (release a (ExitCaseSuccess b))
         pure (b, c)

instance MonadReader Env RIO where
  ask = RIO pure
  local f rio = RIO (runRIO rio . f)

instance MonadUnliftIO RIO where
  askUnliftIO = pure (UnliftIO run)

execRIO :: Env -> RIO a -> IO a
execRIO e rio = runRIO rio e

run :: RIO a -> IO a
run rio = runRIO rio (Env Null)

runLiftIO :: MonadIO m => RIO a -> m a
runLiftIO = liftIO . run

-- environment

newtype Env = Env { ask :: Value }
  deriving (Eq, Show)
