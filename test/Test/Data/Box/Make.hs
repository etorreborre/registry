{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-
  This module tests the construction of some simple values
  using a registry
-}
module Test.Data.Box.Make where

import           Data.Box.Make
import           Data.Text     as T (length)
import           Data.Typeable (Typeable)
import           Protolude

-- | Simple datatypes which can be used in a registry
newtype Text1 = Text1 Text deriving (Eq, Show, Typeable)
newtype Text2 = Text2 Text deriving (Eq, Show, Typeable)
newtype Int1 = Int1 Int deriving (Eq, Show, Typeable)

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




