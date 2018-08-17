{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
  This module contains data structures to describe the
  "warming-up" of boxes in order to ensure that they
   are properly configured:

   - createWarmup creates a warmup from an action
     returning a Result

   - warmupBox takes a box name and unit action
     then just checks that the action executes without
     exception
-}
module Data.Box.Warmup (
  Result(..)
, Warmup
, createWarmup
, declareWarmup
, failed
, isSuccess
, messages
, noWarmup
, ok
, runBoth
, runWarmup
, warmupBox
) where

import qualified Control.Monad.Catch as Catch
import           Data.Box.RIO
import           Data.Semigroup      ((<>))
import           Protolude           as P hiding ((<>))

newtype Warmup =
  Warmup
  { _warmUp :: [RIO Result]
  } deriving (Monoid, Semigroup)

-- * Creation functions

warmupBox :: Text -> RIO () -> Warmup
warmupBox boxName action = createWarmup $
  do res <- Catch.try action :: RIO (Either SomeException ())
     pure $
       case res of
         Left e  -> failed $ "failed to warmup " <> boxName <> show e
         Right _ -> ok $ boxName <> " warmup ok"

createWarmup :: RIO Result -> Warmup
createWarmup t = Warmup [t]

noWarmup :: Warmup
noWarmup = Warmup [pure Empty]

declareWarmup :: Text -> Warmup
declareWarmup boxName = warmupBox boxName (pure ())

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
runWarmup :: Warmup -> RIO Result
runWarmup (Warmup as) = foldr' runBoth (pure Empty) as

-- | runBoth runs both tasks and cumulate the results
--   exceptions are being transformed into Failed results
runBoth :: RIO Result -> RIO Result -> RIO Result
runBoth rio1 rio2 = do
  res1 <- Catch.try rio1 :: RIO (Either SomeException Result)
  res2 <- Catch.try rio2 :: RIO (Either SomeException Result)
  pure $
    case (res1, res2) of
      (Right r1, Right r2) -> r1               `mappend` r2
      (Left  r1, Right r2) -> failed (show r1) `mappend` r2
      (Right r1, Left  r2) -> r1               `mappend` failed (show r2)
      (Left  r1, Left  r2) -> Failed [show r1, show r2]
