{-# LANGUAGE ScopedTypeVariables #-}
module Days.Day20 (day20) where

import Solver
import Control.Arrow
import Data.Maybe
import Data.Foldable
import Data.Array.IArray
import Data.Function
import Data.Map (Map)
import Data.Map qualified as M
import Data.OrdPSQ qualified as OPSQ
import Util.Direction
import Util.List 
import Data.List



addCoord :: Coordinate -> Coordinate -> Coordinate
addCoord (x1, y1) (x2, y2) = (x1+x2, y1+y2)

nextCoord :: (Coordinate, Direction) -> Coordinate
nextCoord (loc, direction) = addCoord loc (directionToCoord direction)


dijkstra :: (Ord i, Eq i, Show i) => (i -> [(i, Int)]) -> i  -> Map i ([i], Int)
dijkstra neighborsOf start = helper M.empty $ OPSQ.singleton start (0 :: Int) [start]
    where
        helper paths q = case OPSQ.minView q of
            Nothing -> paths
            Just (u :: i, udist :: Int, prevs :: [i], q') -> 
                helper (M.insert u (prevs, udist) paths) $ 
                    foldl' (\q'' (v, cost) -> 
                        let altCost = udist + cost
                            addChildren = snd . OPSQ.alter (\case
                                Nothing -> ((), Just (altCost, [u]))
                                Just p'@(curCost, prev') -> ((), case compare altCost curCost of
                                    LT -> Just (altCost, [u]) 
                                    EQ -> Just (altCost, u:prev')
                                    GT -> Just p')
                                ) v
                        in applyWhen (isNothing $ v `M.lookup` paths) addChildren q''
                        ) q' (neighborsOf u)



type Coordinate = (Int,Int)
type Parsed = (Array Coordinate Bool, Coordinate, Coordinate)


day20 :: Solver
day20 = mkSolver 20 "Race Condition" $ (part1 &&& part2) . parser 

part1,part2 :: Parsed -> String
part1 (walls, _, end) = show $ 
    length $ filter (>=100) . fmap snd
    $ r
    where
        distancesFromEnd = dijkstra (neighborsEq False walls) end
        cheats = M.foldlWithKey cheatsAt [] distancesFromEnd
        cheatsAt acc pos (_, posDist) = [((pos, skipsTo), skipsToDist - posDist - 2) 
            | (wall,_) <- neighborsEq True walls pos
            , (skipsTo,_) <- neighborsEq False walls wall
            , skipsTo /= pos
            , Just (_, skipsToDist) <- [distancesFromEnd M.!? skipsTo]
            , skipsToDist > posDist + 2
            ] ++ acc
        r = nubBy ((==) `on` fst) $ sortOn fst cheats

-- allCheats :: Array Coordinate Int -> 
-- allCheats walls distances 


part2 (walls, _, end) = ""
    where
        distancesFromEnd = dijkstra (neighborsEq False walls) end




-- getPaths :: Ord i => i -> Map i ([i],Int) -> [[i]]
-- getPaths x paths = 
--     case x `M.lookup` paths of
--         Just (prevs, _) | prevs == [x] -> [[x]] 
--         Just (prevs :: [i],_) -> do
--             prev <- prevs
--             (x:) <$> getPaths prev paths
--         Nothing -> []

neighborsEq :: Eq a => a -> Array Coordinate a -> Coordinate -> [(Coordinate,Int)]
neighborsEq x grid coord = 
    [(nxt, 1)
        | d <- directions
        , let nxt = nextCoord (coord,d)
        , grid !? nxt == Just x
        ]


parser :: String -> Parsed
parser l = (array ((0,0), (length ls-1, length (head ls)-1)) (fmap (=='#') <$> ixGrid), startPos, endPos)
    where
        ixGrid = indexMatrix ls
        ls = lines l 
        startPos = fst $ fromJust $ find ((=='S') . snd) ixGrid
        endPos = fst $ fromJust $ find ((=='E') . snd) ixGrid
