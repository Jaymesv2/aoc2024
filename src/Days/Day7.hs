module Days.Day7 (day7) where

import Solver

import Control.Arrow
import Data.Int

solver :: [Int64 -> Int64 -> Int64] ->  [(Int64, [Int64])] -> Int64
solver ops  = sum . fmap fst . filter (uncurry elem . fmap combos)
    where 
        combos :: [Int64] -> [Int64]
        combos [] = []
        combos [x] = [x]
        combos (x:y:xs) = ops >>= \f -> combos $ f x y:xs
        -- [u | f <- ops, u <- combos (f x y:xs)]


combine :: Int64 -> Int64 -> Int64
combine a b = ( lengthInt64 b  * a) + b

parse :: String -> [(Int64, [Int64])]
parse = fmap ((read *** fmap read . words . tail ) . span (/=':')) . lines 

day7 :: Solver
day7 = mkSolver 7 "Bridge Repair" $ ( solver  [(+),  (*)] &&& solver [(+),  (*), combine] ) . parse

lengthInt64 :: Int64 -> Int64
lengthInt64 = helper 1 
    where
        helper acc 0 = acc
        helper acc i = helper (acc * 10) (i `quot` 10)
