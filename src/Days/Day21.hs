{-# LANGUAGE ScopedTypeVariables #-}
module Days.Day21 (day21) where

import Solver
import Control.Arrow
-- import Text.Parsec
import Data.Text (Text)
import Debug.Trace
import Data.Function.Memoize
import Util.Parsers
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Data.Array.IArray
import Data.Function
import Data.Map (Map)
import Data.Map qualified as M
import Data.OrdPSQ qualified as OPSQ
import Util.Direction
import Util.List 
import Data.List



day21 :: Solver
day21 = mkSolver 21 "Keypad Conundrum" $ (part1 &&& part2) . parser

part1,part2 :: [String] -> String
part1 _ = "unimplemented"
part2 _ = "unimplemented"

parser :: String -> [String]
parser  = lines

