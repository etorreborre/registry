{-
  Nested datatype to track the resolution algorithm

  From this data type we can draw a graph of the full
  instantation of a value
-}
module Data.Registry.Internal.Operations where

import           Data.Registry.Internal.Types
import           Data.Text as T
import           Protolude

type Operations = [AppliedFunction]
data AppliedFunction = AppliedFunction Value [Value]

makeEdges :: Operations -> [(Value, Value)]
makeEdges [] = []
makeEdges (AppliedFunction out ins : rest) =
  ((out,) <$> ins) <>
  makeEdges rest

-- | A DOT graph
newtype Dot = Dot { unDot :: Text } deriving (Eq, Show)

toDot :: Operations -> Dot
toDot op = Dot $ T.unlines $
  [ "strict digraph {"
  ,  "  node [shape=record]"
  ]
  <> (toDotEdge <$> makeEdges op)
  <> ["}"]

toDotEdge :: (Value, Value) -> Text
toDotEdge (v1, v2) =
     quote (nodeDescription . valDescription $ v1)
  <> " -> "
  <> quote (nodeDescription . valDescription $ v2)
  <> ";"

nodeDescription :: ValueDescription -> Text
nodeDescription (ValueDescription t Nothing) = t
nodeDescription (ValueDescription t (Just v)) = t <> "\n" <> v

quote :: Text -> Text
quote t = "\"" <> (replaceNewlines . removeQuotes) t <> "\""

removeQuotes :: Text -> Text
removeQuotes = T.replace "\"" ""

replaceNewlines :: Text -> Text
replaceNewlines = T.replace "\n" "\\n"
