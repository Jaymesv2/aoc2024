-- {-# LANGUAGE TupleSections #-}
module Days.Day4 (day4) where

import Solver

import Control.Arrow hiding (loop)
import Data.Maybe
import Data.List (transpose, null, tails, isPrefixOf)

day4 :: Solver
day4 = mkSolver 4 "Ceres Search" $ (part1 &&& part2) . lines

part1,part2 :: [String] -> Int
part1 inp = sum $ fmap xmasCount ([id, transpose, diagonals, diagonals . fmap reverse] >>= ($ inp))
part2 = length . filter isxmas2 . matrixWindows 3


xmasCount :: String -> Int
xmasCount = length . filter (\s -> isPrefixOf "XMAS" s || isPrefixOf "SAMX" s) . tails


isxmas2 :: [[Char]] -> Bool
isxmas2 
    (('M':_:'S':_)
    :(_:'A':_:_)
    :('M':_:'S':_)
    :_)
        = True
isxmas2 
    (('S':_:'M':_)
    :(_:'A':_:_)
    :('S':_:'M':_)
    :_)
        = True
isxmas2 
    (('S':_:'S':_)
    :(_:'A':_:_)
    :('M':_:'M':_)
    :_)
        = True
isxmas2 
    (('M':_:'M':_)
    :(_:'A':_:_)
    :('S':_:'S':_)
    :_)
        = True
isxmas2 _ = False


diagonals :: [[a]] -> [[a]]
diagonals = loop 1
    where 
        loop :: Int -> [[a]] -> [[a]]
        loop _ [] = []
        loop i xs = let (r, d) = takeOneFromEach i xs in d:loop (i+1) r

        takeOneFromEach :: Int -> [[a]] -> ([[a]], [a])
        takeOneFromEach i xs = (d ++ b, c)
            where
                (a,b) = splitAt i xs
                c = mapMaybe listToMaybe a
                d = filter (not . null) (drop 1 <$> a)
                


matrixWindows :: Int -> [[a]] -> [[[a]]]
matrixWindows size m = fmap transpose <$> helper1 . transpose =<< helper1 m
    where
        helper1 = filter ( (== size) . length) . fmap (take size) . tails
