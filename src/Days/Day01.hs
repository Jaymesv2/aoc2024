module Days.Day01 (day1) where

import Solver

import Data.List (sort, group)
import Control.Arrow
import Data.Bifunctor

day1 :: Solver
day1 = mkSolver 1 "Historian Hysteria" $ (part1 &&& part2) . bimap sort sort . parse

pairs :: [a] -> [(a,a)]
pairs (a:b:xs) = (a,b):pairs xs
pairs _ = []

parse :: String -> ([Int], [Int])
parse = unzip . pairs . fmap read . words

part1 :: ([Int], [Int]) -> Int
part1 = sum . fmap (abs . uncurry (-)) . uncurry zip 


part2 :: ([Int], [Int]) -> Int
part2 (a,b) = sum . fmap score $ a
    where
        score n = maybe 0 (*n) $ lookup n bgrouped
        bgrouped = (head &&& length) <$> group b
