{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Internal.TypesSpec where

import           Data.List.NonEmpty
import           Data.Registry.Internal.Types
import           Protolude                    as P
import           Test.Tasty.Extensions
import           Type.Reflection

test_specialized_context_order = prop "there are preferrable specializations than other in a given context" $ do
  let c1 = Context (fmap (\t -> (t, Nothing)) $ [f, e, d, c, b, a])
  let s1 = specializedContext c1 (Specialization (a :| [c]) (createValue A))
  let s2 = specializedContext c1 (Specialization (a :| [e]) (createValue A))
  let s3 = specializedContext c1 (Specialization (c :| [f]) (createValue A))
  let s4 = specializedContext c1 (Specialization (b :| [f]) (createValue A))
  let s5 = specializedContext c1 (Specialization (pure c)   (createValue A))
  let s6 = specializedContext c1 (Specialization (pure f)   (createValue A))

  (s2 < s1) === True
  (s3 < s1) === True
  (s4 < s1) === True
  (s3 < s2) === True
  (s4 < s2) === True
  (s4 < s3) === True
  (s1 < s5) === True
  (s6 < s5) === True

data A = A deriving (Eq, Show)
data B = B deriving (Eq, Show)
data C = C deriving (Eq, Show)
data D = D deriving (Eq, Show)
data E = E deriving (Eq, Show)
data F = F deriving (Eq, Show)

a = someTypeRep $ typeOf A
b = someTypeRep $ typeOf B
c = someTypeRep $ typeOf C
d = someTypeRep $ typeOf D
e = someTypeRep $ typeOf E
f = someTypeRep $ typeOf F
