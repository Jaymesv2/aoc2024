module Lib
    ( solvers
    , solutions
    , solutionsTable
    , latestSolution
    , latestSolutionTable
    , solutionN
    , solutionNTable
    ) where

import Solver
import Days.Day01
import Days.Day02
import Days.Day03
import Days.Day04
import Days.Day05
import Days.Day06
import Days.Day07
import Days.Day08
import Days.Day09
import Days.Day10
import Days.Day11
import Days.Day12
import Days.Day13
import Days.Day14
import Days.Day15
import Days.Day16
import Days.Day17
import Days.Day18
import Days.Day19
import Days.Day20
import Days.Day21
import Days.Day22
import Days.Day23
import Days.Day24
import Days.Day25

{- import Data.List (foldl')
import GHC.Conc (par)
inParallel :: [a] -> [a]
inParallel x = foldl' (\x a -> a `par` x) x x  -}

solvers :: [Solver]
solvers = [day1, day2, day3, day4, day5, day6, day7, day8, day9, day10, day11, day12, day13, day14,day15,day16,day17,day18,day19, day20,day21,day22,day23,day24,day25]

solutions :: IO [Solution]
solutions = mapM runSolver solvers

solutionN :: Int -> IO Solution
solutionN n = runSolver (solvers !! n) 

latestSolution :: IO Solution
latestSolution = runSolver $ last solvers

solutionsTable :: IO String
solutionsTable = genTable <$> solutions

solutionNTable :: Int -> IO String
solutionNTable n = genTable . (:[]) <$> solutionN n

latestSolutionTable :: IO String
latestSolutionTable = solutionNTable (length solvers - 1)
