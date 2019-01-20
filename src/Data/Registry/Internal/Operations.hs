{- |
  Nested datatype to track the resolution algorithm

  From this data type we can draw a graph of the full
  instantation of a value
-}
module Data.Registry.Internal.Operations where

import           Data.Hashable
import           Data.Registry.Internal.Types
import           Data.Text                    as T
import           Protolude

-- | A list of function applications created
--   when creating a value out of the Registry
type Operations = [AppliedFunction]

-- | A function application with an output value and a list of input values
data AppliedFunction = AppliedFunction {
    _outputValue :: Value
  , _inputValues ::[Value]
  } deriving (Show)

-- | Make a list of graph edges from the list of function applications
makeEdges :: Operations -> [(Value, Value)]
makeEdges [] = []
makeEdges (AppliedFunction out ins : rest) =
  ((out,) <$> ins) <>
  makeEdges rest

-- * DOT GRAPH

-- | A DOT graph
newtype Dot = Dot {
  unDot :: Text
  } deriving (Eq, Show)

-- | Make a DOT graph out of all the function applications
toDot :: Operations -> Dot
toDot op = Dot $ T.unlines $
  [ "strict digraph {"
  ,  "  node [shape=record]"
  ]
  <> (toDotEdge <$> makeEdges op)
  <> ["}"]

-- | A DOT edge representing the dependency between 2 values
toDotEdge :: (Value, Value) -> Text
toDotEdge (v1, v2) =
     adjust (nodeDescription (valDescription v1)) (valueContext v1)
  <> " -> "
  <> adjust (nodeDescription (valDescription v2)) (valueContext v2)
  <> ";"

valueContext :: Value -> Text
valueContext v =
  let h = hash (unDependencies . valDependencies $ v, specializationContext v)
  in  show . abs $ h

-- | Description of a Value in the DOT graph
nodeDescription :: ValueDescription -> Text
nodeDescription (ValueDescription t Nothing)  = t
nodeDescription (ValueDescription t (Just v)) = t <> "\n" <> v

-- | We need to process the node descriptions
--     - we add quotes arountd the text
--     - we remove quotes (") inside the text
--     - we escape newlines
adjust :: Text -> Text -> Text
adjust node context = "\"" <> (escapeNewlines . removeQuotes) node <> "-" <> context <> "\""

-- | Remove quotes from a textual description to avoid breaking the DOT format
removeQuotes :: Text -> Text
removeQuotes = T.replace "\"" ""

-- | Replace \n with \\n so that newlines are kept in
--   node descriptions
escapeNewlines :: Text -> Text
escapeNewlines = T.replace "\n" "\\n"
