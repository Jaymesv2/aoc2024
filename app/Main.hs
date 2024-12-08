{-# LANGUAGE DeriveDataTypeable, LambdaCase #-}
{-# LANGUAGE GADTs #-} -- temp
module Main (main) where

import Lib
import Data.Typeable
import System.Console.CmdArgs

data AdventOfCode = Day {_day :: Int}
          | Table
          | Latest
          deriving stock (Data,Typeable,Show,Eq)

day', table', latest' :: AdventOfCode
day' = Day
    {_day = def &= typ "Day" &= argPos 0
    } &= help "Print a Day"
table' = Table -- &= auto
latest' = Latest &= auto

main :: IO ()
main = cmdArgs (modes [day', table', latest']) >>= (\case
        Day {_day = d} -> solutionNTable (d-1)
        Table -> solutionsTable
        Latest -> latestSolutionTable) >>= putStrLn
        
