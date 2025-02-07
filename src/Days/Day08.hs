module Days.Day08 (day8) where

import Solver


import Data.List 
import Util.List (groupOn, indexMatrix, combinations)
import Data.Tuple

solution :: ((Int,Int), [(Char, [(Int,Int)])]) -> (Int,Int)
solution ((boundX, boundY), groups) = (solver calcAntinodes, solver calcAntinodes2)
    where
        solver f = length . nub . sort . concatMap (concatMap (filter inBounds . uncurry f) . combinations . snd) $ groups

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


type Coordinate = (Int,Int)

day8 :: Solver
day8 = mkSolver 8 "Resonant Collinearity" $ solution . parse . lines


parse :: [String] -> ((Int,Int), [(Char, [Coordinate])])
parse ls = ((length ls, length (head ls)), groupOn . filter ((/='.') . fst) . fmap swap . indexMatrix $ ls)


