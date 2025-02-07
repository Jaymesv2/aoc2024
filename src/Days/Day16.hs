{-# LANGUAGE ScopedTypeVariables #-}
module Days.Day16 (day16) where

import Solver

import Data.Maybe
import Util.List
import Data.List (nub)
import Data.Foldable
import Data.Array.IArray
import Data.Function

import Data.Map (Map)
import Data.Map qualified as M

import Data.OrdPSQ as OPSQ

import Util.Direction

dijkstra :: (Ord i, Eq i, Show i) => (i -> [(i, Int)]) -> i  -> Map i (i, Int)
dijkstra neighborsOf start = helper M.empty $ OPSQ.singleton start (0 :: Int) start
    where
        helper paths q = case minView q of
            Just (u :: i, udist :: Int, prev :: i, q') -> let 
                paths'' = M.insert u (prev, udist) paths
                q''' = foldl' (\q'' (v, cost) -> 
                    let 
                        altCost = udist + cost
                        vPath = M.lookup v paths
                        (_, alteredQueue) =  
                            OPSQ.alter (\case
                                Nothing -> ((), Just (altCost, u))
                                Just p'@(curCost, _) -> ((), if altCost < curCost then Just (altCost, u) else Just p')
                                ) v q''
                    in if isJust vPath then q'' else alteredQueue
                    ) q' (neighborsOf u)
                in helper paths'' q'''
            Nothing -> paths

addCoord :: Coordinate -> Coordinate -> Coordinate
addCoord (x1, y1) (x2, y2) = (x1+x2, y1+y2)

nextCoord :: (Coordinate, Direction) -> Coordinate
nextCoord (loc, direction) = addCoord loc (directionToCoord direction)


dijkstra2 :: (Ord i, Eq i, Show i) => (i -> [(i, Int)]) -> i  -> Map i ([i], Int)
dijkstra2 neighborsOf start = helper M.empty $ OPSQ.singleton start (0 :: Int) [start]
    where
        helper paths q = case minView q of
            Nothing -> paths
            Just (u :: i, udist :: Int, prevs :: [i], q') -> 
                helper (M.insert u (prevs, udist) paths) $ 
                    foldl' (\q'' (v, cost) -> 
                        let altCost = udist + cost
                            addChildren = snd . OPSQ.alter (\case
                                Nothing -> ((), Just (altCost, [u]))
                                Just p'@(curCost, prev') -> ((), case compare altCost curCost of
                                    LT -> Just (altCost, [u]) 
                                    EQ -> Just (altCost, u:prev')
                                    GT -> Just p')
                                ) v
                        in  applyWhen (isNothing $ v `M.lookup` paths) addChildren q''
                        ) q' (neighborsOf u)

type Coordinate = (Int, Int)
type Parsed = (Array Coordinate Char, Coordinate, Coordinate)

day16 :: Solver
day16 = mkSolver 16 "Reindeer Maze" $ part1 . parse

part1:: Parsed -> (Int, Int)
part1 (grid, start, end) = (endCost, part2) 
    where
        paths = dijkstra2 (neighbors grid) (start, E)
        ends = liftA2 (,) [end] directions
        ( endPos, (_, endCost)) = minimumBy (compare `on` (snd . snd)) $ mapMaybe (\x -> (x,) <$> x `M.lookup` paths ) ends
        path = getPaths endPos paths
        part2 = length . nub . fmap fst $ concat path



getPaths :: Ord i => i -> Map i ([i],Int) -> [[i]]
getPaths x paths = 
    case x `M.lookup` paths of
        Just (prevs, _) | prevs == [x] -> [[x]] 
        Just (prevs :: [i],_) -> do
            prev <- prevs
            (x:) <$> getPaths prev paths
        Nothing -> []

neighbors :: Array Coordinate Char -> (Coordinate, Direction) -> [((Coordinate, Direction), Int)]
neighbors a u@(pos, dir) = 
    (if a ! nxt == '#' then id else ( ((nxt, dir),1):) )
    [((pos, rotateRight dir), 1000 ), ((pos, rotateLeft dir), 1000)]
    where
        nxt = nextCoord u


parse :: String -> Parsed
parse l = (array ((0,0), (length ls-1, length (head ls)-1)) ixGrid, startPos, endPos)
    where
        ixGrid = indexMatrix ls
        ls = lines l 
        startPos = fst $ fromJust $ find ((=='S') . snd) ixGrid
        endPos = fst $ fromJust $ find ((=='E') . snd) ixGrid

