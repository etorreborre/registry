module Test.Data.Box.FileSystem where

import           Prelude   (show)
import           Protolude
import qualified Protolude as P

newtype Box =
  Box
  { readFile :: Text -> IO Text
  }

instance Show Box where
  show = const "file system box"

new :: Box
new = Box (P.readFile . toS)

mock :: [(Text, Text)] -> Box
mock _ = Box
  (const $ pure "text")
