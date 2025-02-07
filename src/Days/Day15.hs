module Days.Day15 (day15) where

import Solver

import Control.Arrow

import Data.Maybe
import Data.Map qualified as M
import Data.Map (Map, fromList)
import Util.List
import Data.Foldable


data Direction = U | R | D | L deriving stock (Show, Eq, Ord, Enum)

directionToCoord :: Direction -> (Int,Int)
directionToCoord U = (-1,0)
directionToCoord D = (1,0)
directionToCoord R = (0,1)
directionToCoord L = (0,-1)

charToDirection :: Char -> Maybe Direction
charToDirection '<' = Just L
charToDirection '>' = Just R
charToDirection '^' = Just U
charToDirection 'v' = Just D
-- extras
charToDirection '[' = Just R
charToDirection ']' = Just L
charToDirection _ = Nothing 

verticalDirection :: Direction -> Bool
verticalDirection U = True
verticalDirection D = True
verticalDirection L = False
verticalDirection R = False

addCoord :: Coordinate -> Coordinate -> Coordinate
addCoord (x1, y1) (x2, y2) = (x1+x2, y1+y2)

nextCoord :: (Coordinate, Direction) -> Coordinate
nextCoord (loc, direction) = addCoord loc (directionToCoord direction)


type Coordinate = (Int, Int)
type Parsed = (Map Coordinate Char, Coordinate, [Direction])



solver :: Parsed -> Int
solver (grid, startPos, ds) =
        M.foldlWithKey (\acc (x,y) v -> if v == 'O' || v == '[' then acc + (100*x + y) else acc) 0 $ snd $ foldl' move (startPos, grid) ds
    where
        move :: (Coordinate, Map Coordinate Char)  -> Direction -> (Coordinate, Map Coordinate Char)
        move acc@(pos, bs) dir 
            | nxtVal == '#' = acc -- cant run into wall
            | nxtVal == '.' = (nxt,bs) -- move to empty space
            | nxtVal == 'O' || nxtVal == '[' || nxtVal == ']' = case pushObj4 '.' bs pos dir of -- push obj at nxt over
                Just bs' -> (nxt, bs') -- move to nxt
                Nothing -> acc -- dont move
            | otherwise = error "tried to move to invalid" --(nxt, bs) -- moving to empty space
            where
                nxt = nextCoord (pos, dir)
                nxtVal = bs M.! nxt

        pushObj4 :: Char ->   Map Coordinate Char -> Coordinate -> Direction -> Maybe (Map Coordinate Char)
        pushObj4 prev grid' pos dir 
            | nxtVal == '#' = Nothing -- runs into a wall, therefore cant push
            | nxtVal == '.' = Just (M.insert nxt prev grid') -- runs into an empty space, can push
            | nxtVal == 'O' = M.insert nxt prev <$> pushObj4 'O' grid' nxt dir -- can it push 1 further

            | nxtVal == '[' && verticalDirection dir = do
                let otherside = nextCoord (nxt, fromJust $ charToDirection '[')
                grid'' <- M.insert nxt prev <$> pushObj4 '[' grid' nxt dir
                M.insert otherside '.' <$> pushObj4 ']' grid'' otherside dir

            | nxtVal == '[' = do
                M.insert nxt prev <$> pushObj4 '[' grid' nxt dir -- can it push 1 further

            | nxtVal == ']' && verticalDirection dir = do
                let otherside = nextCoord (nxt, fromJust $ charToDirection ']')
                grid'' <- M.insert nxt prev <$> pushObj4 ']' grid' nxt dir
                M.insert otherside '.' <$> pushObj4 '[' grid'' otherside dir

            | nxtVal == ']' = do
                M.insert nxt prev <$> pushObj4 ']' grid' nxt dir -- can it push 1 further
            | otherwise = error ""
            where
                nxt = nextCoord (pos, dir)
                nxtVal = grid' M.! nxt

day15 :: Solver
day15 = mkSolver 15 "Warehouse Woes" $ (solver *** solver) . parse

parse :: String -> (Parsed, Parsed)
parse s = ((grid, startPos, directions),(grid2, startPos2, directions))
    where
        (grid_, directions_) = span (/="") $ lines s
        grid2_ =  fmap (>>= f) grid_

        ixGrid = indexMatrix grid_
        startPos = fst $ fromJust $ find ((=='@') . snd) ixGrid
        grid = fromList $ fmap (\x -> if x == '@' then '.' else x) <$> ixGrid

        ixGrid2 = indexMatrix grid2_ 
        startPos2 = fst $ fromJust $ find ((=='@') . snd) ixGrid2
        grid2 = fromList $ fmap (\x -> if x == '@' then '.' else x) <$> ixGrid2

        f :: Char -> [Char]
        f '#' = ['#','#']
        f 'O' = ['[', ']']
        f '.' = ['.','.']
        f '@' = ['@','.']
        f _ = error "invalid Char"

        directions = mapMaybe charToDirection . unlines . tail $ directions_ 


