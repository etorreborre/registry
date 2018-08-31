{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
  This module contains data structures to describe the
  "warming-up" of componnts in order to ensure that they
   are properly configured:

   - createWarmup creates a warmup from an action
     returning a Result

   - warmupOf takes a component name and unit action
     then just checks that the action executes without
     exception
-}
module Data.Registry.Warmup where

import qualified Control.Monad.Catch as Catch
import           Data.Semigroup      ((<>))
import           Protolude           as P hiding ((<>))
import           Data.Typeable

newtype Warmup =
  Warmup
  { _warmUp :: [IO Result]
  } deriving (Monoid, Semigroup)

-- * Creation functions

warmupOf :: Typeable a => a -> IO () -> Warmup
warmupOf a action = createWarmup $
  do res <- Catch.try action :: IO (Either SomeException ())
     pure $
       case res of
         Left e  -> failed $ "KO: " <> show (typeOf a) <> " -> " <> show e
         Right _ -> ok $ "OK: " <> show (typeOf a)

createWarmup :: IO Result -> Warmup
createWarmup t = Warmup [t]

noWarmup :: Warmup
noWarmup = Warmup [pure Empty]

declareWarmup :: Typeable a => a -> Warmup
declareWarmup a = warmupOf a (pure ())

-- | Result of a warmup
data Result =
    Empty
  | Ok [Text]
  | Failed [Text]
  deriving (Eq, Show)

isSuccess :: Result -> Bool
isSuccess Empty      = True
isSuccess (Ok _)     = True
isSuccess (Failed _) = False

ok :: Text -> Result
ok t = Ok [t]

failed :: Text -> Result
failed t = Failed [t]

messages :: Result -> [Text]
messages Empty       = []
messages (Ok ms)     = ms
messages (Failed ms) = ms

instance Monoid Result where
  mempty = Empty
  mappend = (<>)

instance Semigroup Result where
  r         <> Empty      = r
  Empty     <> r          = r
  Failed ts <> r          = Failed (ts ++ messages r)
  r         <> Failed ts  = Failed (messages r ++ ts)
  Ok ts1    <> Ok ts2     = Ok (ts1 ++ ts2)


-- * Run functions

-- | Simple sequential warmup strategy
runWarmup :: Warmup -> IO Result
runWarmup (Warmup as) = foldr' runBoth (pure Empty) as

-- | runBoth runs both tasks and cumulate the results
--   exceptions are being transformed into Failed results
runBoth :: IO Result -> IO Result -> IO Result
runBoth io1 io2 = do
  res1 <- Catch.try io1 :: IO (Either SomeException Result)
  res2 <- Catch.try io2 :: IO (Either SomeException Result)
  pure $
    case (res1, res2) of
      (Right r1, Right r2) -> r1               `mappend` r2
      (Left  r1, Right r2) -> failed (show r1) `mappend` r2
      (Right r1, Left  r2) -> r1               `mappend` failed (show r2)
      (Left  r1, Left  r2) -> Failed [show r1, show r2]
