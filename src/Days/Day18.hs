{-# LANGUAGE ScopedTypeVariables #-}
module Days.Day18 (day18) where

import Solver
import Control.Arrow
import Text.Parsec
import Data.Text (Text)
import Util.Parsers
import Data.Maybe
import Data.Foldable
import Data.Array.IArray
import Data.Function
import Data.Map (Map)
import Data.Map qualified as M
import Data.OrdPSQ qualified as OPSQ
import Util.Direction

addCoord :: Coordinate -> Coordinate -> Coordinate
addCoord (x1, y1) (x2, y2) = (x1+x2, y1+y2)

nextCoord :: (Coordinate, Direction) -> Coordinate
nextCoord (loc, direction) = addCoord loc (directionToCoord direction)


dijkstra2 :: (Ord i, Eq i, Show i) => (i -> Int -> [(i, Int)]) -> i  -> Map i ([i], Int)
dijkstra2 neighborsOf start = helper M.empty $ OPSQ.singleton start (0 :: Int) [start]
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
                        in  applyWhen (isNothing $ v `M.lookup` paths) addChildren q''
                        ) q' (neighborsOf u udist)

type Coordinate = (Int, Int)

day18 :: Solver
day18 = mkParsecSolver 18 "Reindeer Maze" () parseInput $ (part1 &&& part2) . buildArray

part1, part2 :: (Array Coordinate Int, [Coordinate]) -> String
part1 (grid, _) = show $ snd $ paths M.! (size,size)
    where
        paths = dijkstra2 (neighbors startAt grid) (0,0)


size, startAt :: Int
size = 70
startAt = 1024



part2 (grid,coords) = show $ coords !! fst n
    where
        n = fromJust $ find snd $ [(i, nopath i) | i <- [startAt..length coords]]

        nopath :: Int -> Bool
        nopath n' = isNothing $ dijkstra2 (neighbors (n'+1) grid) (0,0) M.!? (size,size)

-- 6,1
neighbors :: Int -> Array Coordinate Int -> Coordinate -> Int -> [(Coordinate,Int)]
neighbors after grid coord _ = 
    [(nxt, 1)
        | d <- directions
        , let nxt = nextCoord (coord,d)
        , Just validBefore <- [grid !? nxt],
        validBefore > after
        ]


-- getPaths :: Ord i => i -> Map i ([i],Int) -> [[i]]
-- getPaths x paths = 
--     case x `M.lookup` paths of
--         Just (prevs, _) | prevs == [x] -> [[x]] 
--         Just (prevs :: [i],_) -> do
--             prev <- prevs
--             (x:) <$> getPaths prev paths
--         Nothing -> []
--
-- printPath :: [Coordinate] -> String
-- printPath path = [if y == size+1 then '\n' else if c `elem` path then 'O' else '.' | c@(_,y) <- range ((0,0), (size,size+1))]


-- printGrid :: Int -> Array Coordinate Int -> String
-- printGrid n grid = [if y == size+1 then '\n' else if n' < n then '#' else '.' | x <- [0..size], y<- [0..size+1], let n' = grid ! (y,x) ]



buildArray :: [Coordinate] -> (Array Coordinate Int, [Coordinate])
buildArray xs = (,xs) $ array ((0,0), (size, size)) [((x,y), fromMaybe maxBound $ (x,y) `lookup` b) | x <- [0..size], y <- [0..size]]
    where
        b = zip xs [1..]

parseInput :: Parsec Text () [Coordinate]
parseInput = ((,) <$> (decimal <* char ',') <*> decimal) `sepEndBy1` char '\n'



-- binarySearch :: (Int -> Bool) -> (Int,Int) -> Int
-- binarySearch cmp = helper 
--     where
--         helper :: (Int,Int) -> Int
--         helper (lower, upper) 
--             | 
--             | otherwise = error ""
--             where 
--                 mid = upper-lower
