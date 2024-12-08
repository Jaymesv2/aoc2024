{-# LANGUAGE TupleSections #-}
module Days.Day5 (day5) where

import Solver

import Control.Arrow
-- import Data.Text (Text)
-- import Data.Either
-- import Data.Functor
-- import Data.Maybe
import Util.List (splitOn)
import Data.List (span, takeWhile, dropWhile, groupBy, intersect, lookup, tails, sortOn, partition, insert, union, sort, sortBy)


middle :: [a] -> a
middle y = middleHelper y y 
    where   
        -- slow, fast
        middleHelper (_:xs) (_:_:ys) = middleHelper xs ys
        middleHelper (x:_) [_] = x
        middleHelper (x:_) [] = x
        middleHelper _ _ = error "empty list"

solution (u,s) = (part1, part2)
    where
        (valid, invalid) = partition (uncurry (==)) . fmap (id &&& sortBy rulesf) $ s
        part1 = sum . fmap (middle . fst) $ valid
        part2 = sum . fmap (middle . snd) $ invalid
        rules = sort u
        rulesf a b = if a == b then EQ else if (a,b) `elem` rules then LT else GT





-- isn't point free style just so much fun :)
parse :: String -> ([(Int,Int)], [[Int]])
parse = (fmap tail . span (/="") . lines) >>> ( fmap ( fmap tail . span (/= '|') >>> (read *** read) ) *** fmap (  fmap read . splitOn (==',') ))

day5 :: Solver
day5 = mkSolver 5 "Print Queue" $ solution . parse

