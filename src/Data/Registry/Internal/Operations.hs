{-
  Nested datatype to track the resolution algorithm

  From this data type we can draw a graph of the full
  instantation of a value
-}
module Data.Registry.Internal.Operations where

import           Data.Registry.Internal.Types
import           Protolude

data Operation =
    Empty
  | RetrievedValue  Value Operation
  | AppliedFunction Value Operation
  deriving (Show)
