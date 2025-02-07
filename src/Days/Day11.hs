module Days.Day11 (day11) where

import Solver

import Control.Arrow
import Data.Function.Memoize

stoneCounter :: Int -> [Int] -> Int
stoneCounter n = sum . fmap (helper n)

helper, helper' :: Int -> Int -> Int
helper = memoize2 helper'
helper' depth n = if depth == 0 then 1 else applyRules (depth-1) n

applyRules :: Int ->  Int -> Int
applyRules 0 _ = 1
applyRules depth 0 = helper depth 1
applyRules depth n 
    | even l = helper depth a + helper depth b
    | otherwise = helper depth $ n*2024
    where 
        (a,b) = divMod n (10^size)
        l = lengthInt64 n
        size = l `div` 2

day11 :: Solver
day11 = mkSolver 11 "Plutonian Pebbles" $ (stoneCounter 25 &&& stoneCounter 75) . fmap read . words

lengthInt64 :: Int -> Int
lengthInt64 = go 0
    where
        go :: Int -> Int -> Int
        go acc 0 = acc
        go acc i = go (acc+1) (i `quot` 10)

