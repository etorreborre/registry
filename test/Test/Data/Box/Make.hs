{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

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

registry1 :: Registry [Int, Int -> Text, Int -> Text -> Text1, Text1 -> Text2]
registry1 =
     int1
  +: add1
  +: add2
  +: toText2
  +: end

countSize :: Text -> Maybe Int
countSize t = Just (T.length t)

made1 :: Text
made1 = make @Text registry1

made2 :: Text1
made2 = make @Text1 registry1

made3 :: Text2
made3 = make @Text2 registry1

--
countSize1 :: Text -> Int1
countSize1 t = Int1 (T.length t)

registry2 :: Registry '[Int, Int -> Text, Text -> Int1]
registry2 =
     int1
  +: add1
  +: countSize1
  +: end

made4 :: Int1
made4 = make @Int1 registry2
