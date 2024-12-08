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
import Days.Day1
import Days.Day2
import Days.Day3
import Days.Day4
import Days.Day5
import Days.Day6
import Days.Day7
import Days.Day8
-- import Days.Day9
-- import Days.Day10
-- import Days.Day11
-- import Days.Day12
-- import Days.Day13
-- import Days.Day14
-- import Days.Day15
-- import Days.Day18
-- import Days.Day19

solvers :: [Solver]
solvers = [day1, day2, day3, day4,day5,day6, day7,day8]
-- day2, day3, day4, day5, day6, day7, day8, day9, day10, day11, day12, day13, day14, day15, day18, day19]

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


