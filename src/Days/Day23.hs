module Days.Day23 (day23) where

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
import Data.Graph


day23 :: Solver
day23 = mkSolver 23 "LAN Party" $ (part1 &&& part2) . parser


part1,part2 :: (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex) -> String
part1 (g, f, fktov) = show $ length groupsOf3
    where
        groupsOf3 = nub $ sort $ [ sort [ v1vertex, v2vertex, v3vertex ]
            | v1vertex <- vertices g
            , let (_,_,neighborsOfV1) = f v1vertex
            , ((v2, v2vertex),(v3, v3vertex)) <- combinations $ mapMaybe (\x -> (x,) <$> fktov x) neighborsOfV1
            , v2 /= v3
            , let (_,_,neighborsOfV2) = f v2vertex
            , let (_,_,neighborsOfV3) = f v3vertex
            , v2 `elem` neighborsOfV3 || v3 `elem` neighborsOfV2
            ]

part2 _ = "unimplemented"

parser :: String -> (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex )
parser s = graphFromEdges [(k,k, snd <$> filter ((==k) . fst) es) | k <- nub $ fst <$> es]
    where
        es = parseLine  <$> lines s
        parseLine l = case splitOn (=='-') l of
            (a:b:_) -> (a,b)
            _ -> error "invalid line"
        

