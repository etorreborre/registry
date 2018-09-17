{- |

 Import this module if you want to access all the functionalities of the
 Registry API

-}
module Data.Registry (
  module M
) where

import Data.Registry.RIO      as M
import Data.Registry.Make     as M
import Data.Registry.Registry as M
import Data.Registry.Lift     as M
import Data.Registry.Solver   as M
import Data.Registry.Warmup   as M
import Data.Registry.Dot      as M
