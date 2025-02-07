module Days.Day10 (day10) where

import Solver

import Data.List 
import Data.Array.IArray as A
import Data.Char
import Util.List (indexMatrix)



type Coordinate = (Int,Int)

solution :: Array Coordinate Int -> Coordinate
solution coords = (part1, part2)
    where

        --ixmap (A.bounds coords) (\b -> rateTrailhead (b, coords ! b) ) coords
        --array (A.bounds coords) $ 

        part1 = sum $ scoreTrailhead <$> trailheads
        part2 = sum $ rateTrailhead <$> trailheads
        -- part2 = sum $ reachableNines . fst <$> trailheads


        -- reachableNines :: (Int,Int) -> Int
        -- reachableNines c@(i,j)
        --     | n == 9 = 1
        --     | otherwise = sum $ reachableNines . fst <$> canStepTo (c,n)
        --     where 
        --         n = coords ! c

        trailheads :: [(Coordinate, Int)]
        trailheads =  filter ((==0) . snd) . assocs $ coords

        rateTrailhead :: (Coordinate, Int) -> Int
        rateTrailhead b = length $ fst <$> walkTrail b

        scoreTrailhead :: (Coordinate, Int) -> Int
        scoreTrailhead b = length $ nub $ fst <$> walkTrail b

        walkTrail :: (Coordinate, Int) -> [(Coordinate,Int)]
        walkTrail b@(_,9) = [b]
        walkTrail b = canStepTo b >>= walkTrail

        canStepTo :: (Coordinate, Int) -> [(Coordinate,Int)]
        canStepTo ((i,j), n) = [(newCoord, n') | newCoord <-  [(i+1,j), (i-1,j), (i,j+1), (i,j-1)] , Just n' <- [coords !? newCoord], n' == n+1]


day10 :: Solver
day10 = mkSolver 10 "Hoof It" $ solution . parse . lines

parse :: [String] -> Array Coordinate Int
parse ls = array ((0,0), (length ls-1, length (head ls)-1)) $ fmap digitToInt <$> indexMatrix ls







