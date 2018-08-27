{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-
  This module tests the construction of some simple values
  using a registry
-}
module Test.Data.Box.Make where

import           Data.Box.Make
import           Data.Text     as T (length)
import           Data.IORef
import           Hedgehog (assert, (===))
import           Protolude hiding (C1)
import           Test.Tasty
import           Test.Tasty.TH
import           Test.Tasty.Extensions
import           System.IO.Memoize

-- | Modification of stored values

-- | Contextual setting of different values for a given type
test_contextual = test "boxes can use some values depending on some context" $ do
  (c1, c2) <- liftIO $
    do let r =    Config 3
               +: newUseConfig1
               +: newUseConfig2
               +: end
       let r' = specialize @UseConfig1 (Config 1) $
                specialize @UseConfig2 (Config 2) $ r
       pure (printConfig1 (make @UseConfig1 r'), printConfig2 (make @UseConfig2 r'))

  c1 === Config 1
  c2 === Config 2

newtype Config = Config Int deriving (Eq, Show)

newtype UseConfig1 = UseConfig1 { printConfig1 :: Config }
newUseConfig1 config = UseConfig1 { printConfig1 = config }

newtype UseConfig2 = UseConfig2 { printConfig2 :: Config }
newUseConfig2 config = UseConfig2 { printConfig2 = config }

-- | Creation of singletons with memoization
test_singleton = test "boxes can be made with singletons with System.IO.Memoize" $ do
  (c1, c2) <- liftIO $
    do -- create a counter for the number of instantiations
       counter <- newIORef 0

       newSingOnce <- once (newSing counter)
       let r =    (intoM @IO newC1)
               +: (intoM @IO newC2)
               +: newSingOnce
               +: end
       c1 <- make @(IO C1) r
       c2 <- make @(IO C2) r
       pure (c1, c2)

  c1 === C1 (Sing 1)
  c2 === C2 (Sing 1)

newtype C1 = C1 Sing deriving (Eq, Show)
newC1 :: Sing -> IO C1
newC1 = pure . C1

newtype C2 = C2 Sing deriving (Eq, Show)
newC2 :: Sing -> IO C2
newC2 = pure . C2

newtype Sing = Sing Int deriving (Eq, Show)

newSing :: IORef Int -> IO Sing
newSing counter = do
  _ <- modifyIORef counter (+1)
  i <- readIORef counter
  pure (Sing i)

test_cycle = test "cycle can be detected" $ do
  -- a registry with 2 functions inverse of each other
  let explosive = makeUnsafe @Text (add1 +: dda1 +: end)
  r <- liftIO $ try (print explosive)
  case r of
    Left (_ :: SomeException) -> assert True
    Right _ -> assert False

-- | A regular module can be made without having an explicit Typeable constraint
data LoggingModule = LoggingModule {
  info  :: Text -> IO ()
, debug :: Text -> IO ()
}

loggingModule = make @LoggingModule (LoggingModule { info = print, debug = print } +: end)

-- | Simple datatypes which can be used in a registry
newtype Text1 = Text1 Text deriving (Eq, Show)
newtype Text2 = Text2 Text deriving (Eq, Show)
newtype Int1 = Int1 Int deriving (Eq, Show)

-- | values and functions
int1 :: Int
int1 = 1

add1 :: Int -> Text
add1 i = show (i + 1)

add2 :: Int -> Text -> Text1
add2 i j = Text1 (show (i+1) <> j)

text1 :: Text
text1 = "text1"

toText2 :: Text1 -> Text2
toText2 (Text1 t) = (Text2 t)

registry1 :: Registry (Union (Inputs Int)  '[Int, Text, Text1, Int])
                      (Union '[Output Int] '[Text1, Text2, Text])
registry1 =
     int1
  +: add1
  +: add2
  +: toText2
  +: end

countSize :: Text -> Maybe Int
countSize t = Just (T.length t)

m = make @Text $ (\(t::Text) -> t) +: end


made1 :: Text
made1 = make @Text registry1

made2 :: Text1
made2 = make @Text1 registry1

made3 :: Text2
made3 = make @Text2 registry1

--
countSize1 :: Text -> Int1
countSize1 t = Int1 (T.length t)

registry2 :: Registry (Union (Inputs Int)  '[Text, Int])
                      (Union '[Output Int] '[Int1, Text])
registry2 =
     int1
  +: add1
  +: countSize1
  +: end

made4 :: Int1
made4 = make @Int1 registry2

-- | This does *not* compile because Double in not in the
--   list of outputs for registry2
{-
wrong :: Double
wrong = make @Double registry2
-}

-- | This does *not* compile because the list of inputs
--   in registry2 is not included in the list of outputs
unknown :: Double -> Text1
unknown _ = Text1 "text1"

registry3 :: Registry (Union (Inputs Int) '[Int, Text, Double])
                      (Union '[Output Int] '[Text, Int1, Text1])
registry3 =
     int1
  +: unknown
  +: add1
  +: countSize1
  +: end

-- | This does not compile because we need a double
--   to make Text1 and it is not in the list of outputs
{-
wrong :: Text1
wrong = make @Text1 registry3
-}

-- | This version compiles but throws an exception at runtime
dangerous :: Text1
dangerous = makeUnsafe @Text1 registry3

-- | This test shows that we can detect a cycle at runtime

-- inverse of add1
dda1 :: Text -> Int
dda1 = T.length

----
tests = $(testGroupGenerator)
