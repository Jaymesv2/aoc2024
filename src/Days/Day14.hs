module Days.Day14 (day14) where

import Solver

import Control.Arrow
import Data.Text (Text)

import Text.Parsec
-- import Data.Maybe
import Data.Functor
import Data.Char
import Debug.Trace

type Coordinate = (Int', Int')
type Int' = Int

xDimention, yDimention :: Int'
xDimention = 11
yDimention = 7
-- xDimention = 101
-- yDimention = 103

locationAfterNSteps :: Int' -> (Coordinate, Coordinate) -> (Int,Int)
locationAfterNSteps steps ((px,py), (vx,vy)) = (px', py')
    where
        px' = (px+vx*steps) `mod` xDimention
        py' = py+vy*steps  `mod` (yDimention+1)



showB :: [Coordinate] -> String
showB lst = [ if x == xDimention+1 then '\n' else if d == 0 then '.' else intToDigit d | y <- [0..yDimention],  x <- [0..xDimention+1], let d = length $ filter (==(x,y)) lst] 


perQuadrant :: [Coordinate] -> Int'
-- ([Coordinate], [Coordinate], [Coordinate], [Coordinate])
perQuadrant xs = q1*q2*q3*q4
    where
        q1 = length [x | x@(px,py) <- xs, px < xDimention `div` 2, py < yDimention `div` 2]
        q2 = length [x | x@(px,py) <- xs, px < xDimention `div` 2, py > yDimention `div` 2]
        q3 = length [x | x@(px,py) <- xs, px > xDimention `div` 2, py < yDimention `div` 2]
        q4 = length [x | x@(px,py) <- xs, px > xDimention `div` 2, py > yDimention `div` 2]


part1,part2 :: [ (Coordinate, Coordinate) ] -> String
part1 s =  show (perQuadrant $ ( \x -> trace (showB x) x) $ fmap (locationAfterNSteps 100) s) 
    -- sum . fmap (uncurry (+) . first (*3)) . filter (\(x,y) -> x <= 100 && y <= 100) . mapMaybe solveEqn 

part2 _ =  ""
    -- sum .
    --  fmap ( uncurry (+) . first (*3) ) 
    -- . mapMaybe ( solveEqn 


day14 :: Solver
day14 = mkParsecSolver 14 "Restroom Redoubt" () parseInput (part1 &&& part2)


parseInput :: Parsec Text () [ (Coordinate, Coordinate) ]
parseInput = do
    flip sepEndBy1 (try $ char '\n') $ do 
        _ <- string "p=" 
        p <- coordinate
        _ <- string " v="
        v <- coordinate
        pure (p,v)
    where 
        number = option id (char '-' $> negate) <*> (read <$> many1 digit)
        coordinate = (,) <$> number <* char ',' <*> number



