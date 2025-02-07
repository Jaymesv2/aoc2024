module Days.Day05 (day5) where

import Solver

import Control.Arrow
import Util.List (splitOn)
import Data.List (partition, sortBy, sort)


middle :: [a] -> a
middle y = middleHelper y y 
    where   
        -- slow, fast
        middleHelper (_:xs) (_:_:ys) = middleHelper xs ys
        middleHelper (x:_) [_] = x
        middleHelper (x:_) [] = x
        middleHelper _ _ = error "empty list"

solution :: ([(Int,Int)], [[Int]]) -> (Int,Int)
solution (u,s) = (part1, part2)
    where
        (valid, invalid) = partition (uncurry (==)) . fmap (id &&& sortBy rulesf) $ s
        part1 = sum . fmap (middle . fst) $ valid
        part2 = sum . fmap (middle . snd) $ invalid
        rules = sort u
        rulesf a b
            | a == b = EQ 
            | (a,b) `elem` rules = LT 
            | otherwise = GT


-- isn't point free style just so much fun :)
parse :: String -> ([(Int,Int)], [[Int]])
parse = (fmap tail . span (/="") . lines) >>> ( fmap ( fmap tail . span (/= '|') >>> (read *** read) ) *** fmap (  fmap read . splitOn (==',') ))

day5 :: Solver
day5 = mkSolver 5 "Print Queue" $ solution . parse

