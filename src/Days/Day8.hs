module Days.Day8 (day8) where

import Solver

import Control.Arrow
import Data.Int

import Data.Text (Text)
-- import Data.Either
import Data.Functor
import Data.Maybe
import Data.List 
import Debug.Trace
import Util.List (groupOn)
-- import Text.Parsec

-- part1 :: ((Int,Int), [(Char, [Coordinate])]) -> String
solution (bounds@(boundX, boundY), groups) = (solver calcAntinodes, solver calcAntinodes2)
    where

        solver f = length . nub . sort . concatMap (concatMap (filter inBounds . uncurry f) . combinations . snd) $ groups

        -- ungroups = concatMap (\(c, coords) -> (c,) <$> coords) groups
        --
        -- printGrid :: [Coordinate] -> String
        -- printGrid visited = 
        --     [  if y == boundY then '\n'
        --         else if (x,y) `elem` visited then 'X'
        --             else case find (\(_, coord) -> (x,y) == coord) ungroups of
        --                 Just (c,_) -> c
        --                 Nothing -> '.'
        --         | x <- [0..boundX-1], y <- [0..boundY]]

        oob (x,y) = x < 0 || y < 0 || x >= boundX || y >= boundY
        inBounds = not . oob


        calcAntinodes2 :: Coordinate -> Coordinate -> [Coordinate]
        calcAntinodes2 c1@(x1, y1) c2@(x2, y2) = c1:c2:(a ++ b)
            where 
                a = takeWhile inBounds $ iterate (\(x,y) -> (x+dx, y+dy)) c1 
                b = takeWhile inBounds $ iterate (\(x,y) -> (x-dx, y-dy)) c2 
                    
                dx = x1 - x2
                dy = y1 - y2
            
        calcAntinodes :: Coordinate -> Coordinate -> [Coordinate]
        calcAntinodes (x1, y1) (x2, y2) = [(x1 + dx, y1 + dy), (x2 - dx, y2 - dy)]
            where 
                dx = x1 - x2
                dy = y1 - y2

combinations :: [a] -> [(a,a)]
combinations [] = []  
combinations (x:xs) = fmap (x,) xs ++ combinations xs


type Coordinate = (Int,Int)

day8 :: Solver
day8 = mkSolver 8 "Resonant Collinearity" $ solution . parse . lines


parse :: [String] -> ((Int,Int), [(Char, [Coordinate])])
parse ls = ((length ls, length (head ls)), groupOn . filter ((/='.') . fst) . indexMatrix $ ls)
    where
        indexMatrix :: [[a]] -> [(a,(Int, Int))]
        indexMatrix xs = [ (y, (ix, iy)) | (ys, ix) <- zip xs [0..], (y, iy) <- zip ys [0..] ]


